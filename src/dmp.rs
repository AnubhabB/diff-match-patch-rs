use std::time::{Duration, Instant};


/**
 * The data structure representing a diff is an array of tuples:
 * [[DIFF_DELETE, 'Hello'], [DIFF_INSERT, 'Goodbye'], [DIFF_EQUAL, ' world.']]
 * which means: delete 'Hello', add 'Goodbye' and keep ' world.'
 */

/// Enum representing the different ops of diff
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(i8)]
pub enum Ops {
    Delete = -1,
    Insert,
    Equal
}

/// A structure representing a diff
/// (Ops::Delete, String::new("Hello")) means delete `Hello`
/// (Ops::Insert, String::new("Goodbye")) means add `Goodbye`
/// (Ops::Equal, String::new("World")) means keep world
#[derive(Debug, PartialEq, Eq)]
pub struct Diff(Ops, String);

impl Diff {
    /// Create a new diff object
    pub fn new(op: Ops, text: &[u8]) -> Self {
        Self(op, String::from_utf8(text.to_vec()).unwrap())
    }
}

pub struct Patch {}

pub type Patches = Vec<Patch>;

pub struct DiffMatchPatch {
    /// a speedup flag, If present and false, then don't run
    /// a line-level diff first to identify the changed areas.
    /// Defaults to true, which does a faster, slightly less optimal diff.
    checklines: Option<bool>,
    /// A default timeout in num seconds, defaults to 1
    timeout: Option<u64>
}

impl Default for DiffMatchPatch {
    fn default() -> Self {
        Self {
            checklines: Some(true),
            timeout: Some(1)
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct HalfMatch<'a> {
    prefix_long: &'a [u8],
    suffix_long: &'a [u8],
    prefix_short: &'a [u8],
    suffix_short: &'a [u8],
    common: &'a [u8]
}

impl DiffMatchPatch {
    fn checklines(&self) -> bool {
        self.checklines.map_or(true, |c| c)
    }

    // returns the configured timeout, defaults to `1`, None or `0` would mean infinite timeout
    fn timeout(&self) -> u64 {
        self.timeout.map_or(u64::MAX, |tout| if tout >  0 { tout } else { u64::MAX })
    }

    fn diff_main_internal(&self, old_bytes: &[u8], new_bytes: &[u8]) -> Vec<Diff> {
        // First, check if lhs and rhs are equal
        if old_bytes == new_bytes {
            if old_bytes.is_empty() {
                return Vec::new();
            }

            return vec![Diff::new(Ops::Equal, old_bytes)];
        }

        if old_bytes.is_empty() {
            return vec![Diff::new(Ops::Insert, new_bytes)]
        }

        if new_bytes.is_empty() {
            return vec![Diff::new(Ops::Delete, old_bytes)]
        }

        let deadline = Instant::now().checked_add(Duration::from_secs(self.timeout())).unwrap();

        // Trim common prefix
        let common_prefix = Self::common_prefix(old_bytes, new_bytes, false);
        let common_suffix = Self::common_prefix(&old_bytes[common_prefix..], &new_bytes[common_prefix..], true);

        let mut diffs = self.diff_compute(&old_bytes[common_prefix .. old_bytes.len() - common_suffix], &new_bytes[common_prefix .. new_bytes.len() - common_suffix]);

        // Restore the prefix and suffix.
        if common_prefix > 0 {
            let mut d = vec![Diff::new(Ops::Equal, &old_bytes[.. common_prefix])];
            d.append(&mut diffs);
            diffs = d;
        }

        if common_suffix > 0 {
            diffs.push(Diff::new(Ops::Equal, &new_bytes[new_bytes.len() - common_suffix ..]));
        }

        
        diffs
    }

    
    fn diff_compute(&self, old: &[u8], new: &[u8]) -> Vec<Diff> {
        // returning all of the new part
        if old.is_empty() {
            return vec![Diff::new(Ops::Insert, new)]
        }

        // return everything deleted
        if new.is_empty() {
            return vec![Diff::new(Ops::Delete, old)]
        }

        let (long, short, old_gt_new) = if old.len() > new.len() { (old, new, true) } else { (new, old, false) };

        // found a subsequence which contains the short text
        if let Some(idx) = long.windows(short.len()).step_by(1).position(|k| k == short) {
            // Shorter text is inside the longer text (speedup).
            let op = if old_gt_new { Ops::Delete } else { Ops::Insert };
            let diffs = vec![
                Diff::new(op, &long[0 .. idx]),
                Diff::new(Ops::Equal, short),
                Diff::new(op, &long[idx + short.len() ..])
            ];

            return diffs;
        }

        if short.len() == 1 {
            // After previous case, this can't be an equality
            return vec![Diff::new(Ops::Delete, old), Diff::new(Ops::Insert, new)];
        }
        
        // Check if the problem can be split in two
        if let Some(half_match) = self.diff_half_match(old, new) {
            let old_a = half_match.prefix_long;
            let old_b = half_match.suffix_long;
            
            let new_a = half_match.prefix_short;
            let new_b = half_match.suffix_short;

            let mid_common = half_match.common;

            // Send both pairs off for separate processing.
            let mut diffs_a = self.diff_main_internal(old_a, new_a);
            let mut diffs_b = self.diff_main_internal(old_b, new_b);

            // Merge the results
            diffs_a.push(Diff::new(Ops::Equal, mid_common));
            diffs_a.append(&mut diffs_b);
            
            return diffs_a;
        }
        
        if self.checklines() && old.len() > 100 && new.len() > 100 {
            return self.diff_line_mode(old, new);
        }

        self.diff_bisect(old, new)
    }

    fn diff_half_match<'a>(&self, old: &'a [u8], new: &'a [u8]) -> Option<HalfMatch<'a>> {
        // Don't risk returning a suboptimal diff when we have unlimited time
        if self.timeout() == u64::MAX {
            return None;
        }

        let (long, short) = if old.len() > new.len() { (old, new) } else { (new, old) };

        // pointless - two small for this algo
        if long.len() < 4 || short.len() * 2 < long.len() {
            return None;
        }

        // First check if the second quarter is the seed for a half-match.
        // let hm1 = Self::diff_half_match_i(long, short, (long.len() as f32 / 4.).ceil() as usize);
        let hm1 = Self::diff_half_match_i(long, short, long.len() / 4 );
        // Check again based on the third quarter.
        // let hm2 = Self::diff_half_match_i(long, short, (long.len() as f32 / 2.).ceil() as usize);
        let hm2 = Self::diff_half_match_i(long, short, long.len() / 2 );

        if hm1.is_none() && hm2.is_none() {
            return None;
        }

        let hm = if let (Some(hm1), None) = (&hm1, &hm2) {
            hm1
        } else if let (None, Some(hm2)) = (&hm1, &hm2) {
            hm2
        } else if let (Some(hm1), Some(hm2)) = (&hm1, &hm2) {
            // both match, select the longest
            if hm1.common.len() > hm2.common.len() {
                hm1
            } else {
                hm2
            }
        } else {
            return None
        };


        // A half-match was found, sort out the return data.
        let half_match = if old.len() > new.len() {
            HalfMatch {
                prefix_long: hm.prefix_long,
                suffix_long: hm.suffix_long,
                prefix_short: hm.prefix_short,
                suffix_short: hm.suffix_short,
                common: hm.common
            }
        } else {
            HalfMatch {
                prefix_long: hm.prefix_short,
                suffix_long: hm.suffix_short,
                prefix_short: hm.prefix_long,
                suffix_short: hm.suffix_long,
                common: hm.common
            }
        };
        
        Some(half_match)
    }

    fn diff_line_mode(&self, old: &[u8], new: &[u8]) -> Vec<Diff> {
        todo!()
    }

    fn diff_bisect(&self, old: &[u8], new: &[u8]) -> Vec<Diff> {
        todo!()
    }
    
    // Does a substring of shorttext exist within longtext such that the substring
    // is at least half the length of longtext?
    //idx Start index of quarter length substring within longtext.
    fn diff_half_match_i<'a>(long: &'a[u8], short: &'a[u8], idx: usize) -> Option<HalfMatch<'a>> {
        // Start with a 1/4 length substring at position i as a seed.

        let seed = &long[idx .. idx + long.len() / 4];
        let mut j = 0;

        let mut best_common: &[u8] = &[];
        let mut best_long_a: &[u8] = &[];
        let mut best_long_b: &[u8] = &[];
        let mut best_short_a: &[u8] = &[];
        let mut best_short_b: &[u8] = &[];


        while let Some(pos) = &short[ j .. ]
            .windows(seed.len())
            .step_by(1)
            .position(|p| p == seed) {
                j += *pos;

                let prefix_len = Self::common_prefix(&long[idx ..], &short[j ..], false);
                let suffix_len = Self::common_prefix(&long[.. idx], &short[.. j], true);

                if best_common.len() < suffix_len + prefix_len {
                    best_common = &short[j - suffix_len .. j + prefix_len];
                    
                    best_long_a = &long[.. idx - suffix_len];
                    best_long_b = &long[idx + prefix_len ..];

                    best_short_a = &short[.. j - suffix_len];
                    best_short_b = &short[j + prefix_len ..];
                }

                j += 1;
        }

        if best_common.len() * 2 >= long.len() {
            Some(
                HalfMatch {
                    prefix_long: best_long_a,
                    suffix_long: best_long_b,
                    prefix_short: best_short_a,
                    suffix_short: best_short_b,
                    common: best_common
                }
            )
        } else {
            None
        }
    }

    // returns the number of bytes common in both the str - this is the position in bytes not chars, [0 .. n] is your prefix
    // We are doing a binary search here, and I've observed similar performance as noted by https://neil.fraser.name/news/2007/10/09/
    // Some benchmark code can be found in benches/prefix.rs
    // Reverse prefix is suffix
    // TODO: investigate this further
    fn common_prefix(lhs: &[u8], rhs: &[u8], reverse: bool) -> usize {
        if lhs.is_empty() || rhs.is_empty() ||
            (!reverse && (lhs.first() != rhs.first())) ||
            (reverse && (lhs.last() != rhs.last())) {
                return 0;
            }


        let mut pointmin = 0;
        let mut pointmax = lhs.len().min(rhs.len());
        let mut pointmid = pointmax;

        let mut pointstart = 0;

        while pointmin < pointmid {
            let (lhsrange, rhsrange) = if !reverse {
                (pointstart .. pointmid, pointstart .. pointmid)
            } else {
                (lhs.len() - pointmid .. lhs.len() - pointstart, rhs.len() - pointmid .. rhs.len() - pointstart)
            };

            if lhs[lhsrange] == rhs[rhsrange] {
                    pointmin = pointmid;
                    pointstart = pointmin;
            } else {
                pointmax = pointmid;
            }

            pointmid = (pointmax - pointmin) / 2 + pointmin;
        }

        pointmid
    }

    
}

impl DiffMatchPatch {
    /// Find the differences between two texts.  Simplifies the problem by stripping any common prefix or suffix off the texts before diffing.
    /// Args:
    /// old: Old string to be diffed.
    /// new: New string to be diffed.
    /// deadline: Optional time when the diff should be complete by.  Used
    /// internally for recursive calls.  Users should set DiffTimeout instead.
    /// 
    /// Returns:
    /// Vec of changes (Diff).
    pub fn diff_main(&self, old: &str, new: &str) -> Vec<Diff> {
        self.diff_main_internal(old.as_bytes(), new.as_bytes())
    }

    pub fn diff_cleanup_semantic(diffs: &mut [Diff]) {
        todo!()
    }

    pub fn diff_cleanup_efficiency(diffs: &mut [Diff]) {
        todo!()
    }

    pub fn diff_levenshtein(diffs: &[Diff]) -> u32 {
        todo!()
    }

    pub fn diff_pretty_html(diffs: &[Diff]) -> String {
        todo!()
    }

    pub fn match_main(text: &str, pattern: &str, loc: ()) -> () {
        todo!()
    }

    pub fn patch_make_text_text(text1: &str, text2: &str) -> Patches {
        todo!()
    }

    pub fn patch_make_diff(diffs: &[Diff]) -> Patches {
        todo!()
    }

    pub fn patch_make_text_diff(text1: &str, diffs: &[Diff]) -> Patches {
        todo!()
    }

    pub fn patch_to_text(patches: Patches) -> String {
        todo!()
    }

    pub fn patch_from_text(text: &str) -> Patches {
        todo!()
    }

    pub fn patch_apply(patches: &[Patch], text: &str) -> (String, ()) {
        todo!()
    }
}


#[cfg(test)]
mod tests {
    use std::u64;

    use crate::dmp::{Diff, HalfMatch, Ops};

    use super::DiffMatchPatch;

    // const tests = [
    //     'testDiffIsDestructurable', // TODO
    //     'testDiffCommonOverlap',
    //     'testDiffLinesToChars',
    //     'testDiffCharsToLines',
    //     'testDiffCleanupMerge',
    //     'testDiffCleanupSemanticLossless',
    //     'testDiffCleanupSemantic',
    //     'testDiffCleanupEfficiency',
    //     'testDiffPrettyHtml',
    //     'testDiffText',
    //     'testDiffDelta',
    //     'testDiffXIndex',
    //     'testDiffLevenshtein',
    //     'testDiffBisect',
    //     'testMatchAlphabet',
    //     'testMatchBitap',
    //     'testMatchMain',
    //     'testPatchObj',
    //     'testPatchFromText',
    //     'testPatchToText',
    //     'testPatchAddContext',
    //     'testPatchMake',
    //     'testPatchSplitMax',
    //     'testPatchAddPadding',
    //     'testPatchApply'
    // ];


    #[test]
    fn test_diff_is_destructurable() {

    }

    #[test]
    fn test_prefix() {
        // Detect any common prefix.
        // Null case.
        assert_eq!(0, DiffMatchPatch::common_prefix("abc".as_bytes(), "xyz".as_bytes(), false));

        // Non-null case.
        assert_eq!(4, DiffMatchPatch::common_prefix("1234abcdef".as_bytes(), "1234xyz".as_bytes(), false));

        // Whole case.
        assert_eq!(4, DiffMatchPatch::common_prefix("1234".as_bytes(), "1234xyz".as_bytes(), false));
    }

    #[test]
    fn test_suffix() {
        // Detect any common suffix.
        // Null case.
        assert_eq!(0, DiffMatchPatch::common_prefix("abc".as_bytes(), "xyz".as_bytes(), true));

        // Non-null case.
        assert_eq!(4, DiffMatchPatch::common_prefix("abcdef1234".as_bytes(), "xyz1234".as_bytes(), true));

        // Whole case.
        assert_eq!(4, DiffMatchPatch::common_prefix("1234".as_bytes(), "xyz1234".as_bytes(), true));
    }

    #[test]
    fn test_diff_half_match() {
        let mut dmp = DiffMatchPatch::default();

        // No match
        assert!(dmp.diff_half_match("1234567890".as_bytes(), "abcdef".as_bytes()).is_none());
        assert!(dmp.diff_half_match("12345".as_bytes(), "23".as_bytes()).is_none());

        // Single Match.
        assert_eq!(
            Some(HalfMatch { prefix_long: "12".as_bytes(), suffix_long: "90".as_bytes(), prefix_short: "a".as_bytes(), suffix_short: "z".as_bytes(), common: "345678".as_bytes() }),
            dmp.diff_half_match("1234567890".as_bytes(), "a345678z".as_bytes())
        );
        assert_eq!(
            Some(HalfMatch { prefix_long: "a".as_bytes(), suffix_long: "z".as_bytes(), prefix_short: "12".as_bytes(), suffix_short: "90".as_bytes(), common: "345678".as_bytes() }),
            dmp.diff_half_match("a345678z".as_bytes(), "1234567890".as_bytes())
        );
        assert_eq!(
            Some(HalfMatch { prefix_long: "abc".as_bytes(), suffix_long: "z".as_bytes(), prefix_short: "1234".as_bytes(), suffix_short: "0".as_bytes(), common: "56789".as_bytes() }),
            dmp.diff_half_match("abc56789z".as_bytes(), "1234567890".as_bytes())
        );
        assert_eq!(
            Some(HalfMatch { prefix_long: "a".as_bytes(), suffix_long: "xyz".as_bytes(), prefix_short: "1".as_bytes(), suffix_short: "7890".as_bytes(), common: "23456".as_bytes() }),
            dmp.diff_half_match("a23456xyz".as_bytes(), "1234567890".as_bytes())
        );

        // Multiple Matches.
        assert_eq!(
            Some(HalfMatch { prefix_long: "12123".as_bytes(), suffix_long: "123121".as_bytes(), prefix_short: "a".as_bytes(), suffix_short: "z".as_bytes(), common: "1234123451234".as_bytes() }),
            dmp.diff_half_match("121231234123451234123121".as_bytes(), "a1234123451234z".as_bytes())
        );
        assert_eq!(
            Some(HalfMatch { prefix_long: "".as_bytes(), suffix_long: "-=-=-=-=-=".as_bytes(), prefix_short: "x".as_bytes(), suffix_short: "".as_bytes(), common: "x-=-=-=-=-=-=-=".as_bytes() }),
            dmp.diff_half_match("x-=-=-=-=-=-=-=-=-=-=-=-=".as_bytes(), "xx-=-=-=-=-=-=-=".as_bytes())
        );
        assert_eq!(
            Some(HalfMatch { prefix_long: "-=-=-=-=-=".as_bytes(), suffix_long: "".as_bytes(), prefix_short: "".as_bytes(), suffix_short: "y".as_bytes(), common: "-=-=-=-=-=-=-=y".as_bytes() }),
            dmp.diff_half_match("-=-=-=-=-=-=-=-=-=-=-=-=y".as_bytes(), "-=-=-=-=-=-=-=yy".as_bytes())
        );

        // Non-optimal halfmatch.
        // Optimal diff would be -q+x=H-i+e=lloHe+Hu=llo-Hew+y not -qHillo+x=HelloHe-w+Hulloy
        assert_eq!(
            Some(HalfMatch { prefix_long: "qHillo".as_bytes(), suffix_long: "w".as_bytes(), prefix_short: "x".as_bytes(), suffix_short: "Hulloy".as_bytes(), common: "HelloHe".as_bytes() }),
            dmp.diff_half_match("qHilloHelloHew".as_bytes(), "xHelloHeHulloy".as_bytes())
        );

        // Optimal no halfmatch.
        dmp.timeout = Some(u64::MAX);
        assert!(dmp.diff_half_match("qHilloHelloHew".as_bytes(), "xHelloHeHulloy".as_bytes()).is_none());
    }

    #[test]
    fn test_diff_main() {
        let dmp = DiffMatchPatch::default();

        // Perform a trivial diff.
        // Null case.
        assert!(dmp.diff_main("", "").is_empty());
        
        // Equality
        assert_eq!(vec![Diff::new(Ops::Equal, "abc".as_bytes())], dmp.diff_main("abc", "abc"));

        // Simple insert
        assert_eq!(
            vec![Diff::new(Ops::Equal, "ab".as_bytes()), Diff::new(Ops::Insert, "123".as_bytes()), Diff::new(Ops::Equal, "c".as_bytes())],
            dmp.diff_main("abc", "ab123c")
        );

        // Simple delete
        assert_eq!(
            vec![Diff::new(Ops::Equal, "a".as_bytes()), Diff::new(Ops::Delete, "123".as_bytes()), Diff::new(Ops::Equal, "bc".as_bytes())],
            dmp.diff_main("a123bc", "abc")
        );

        // Two insertions
        // assert_eq!(
        //     vec![
        //         Diff::new(Ops::Equal, "a".as_bytes()),
        //         Diff::new(Ops::Insert, "123".as_bytes()),
        //         Diff::new(Ops::Equal, "b".as_bytes()),
        //         Diff::new(Ops::Insert, "456".as_bytes()),
        //         Diff::new(Ops::Equal, "c".as_bytes()),
        //     ],
        //     dmp.diff_main("abc", "a123b456c")
        // );

//   // Two insertions.
//   assertEquivalent([[DIFF_EQUAL, 'a'], [DIFF_INSERT, '123'], [DIFF_EQUAL, 'b'], [DIFF_INSERT, '456'], [DIFF_EQUAL, 'c']], dmp.diff_main('abc', 'a123b456c', false));

//   // Two deletions.
//   assertEquivalent([[DIFF_EQUAL, 'a'], [DIFF_DELETE, '123'], [DIFF_EQUAL, 'b'], [DIFF_DELETE, '456'], [DIFF_EQUAL, 'c']], dmp.diff_main('a123b456c', 'abc', false));

//   // Perform a real diff.
//   // Switch off the timeout.
//   dmp.Diff_Timeout = 0;
//   // Simple cases.
//   assertEquivalent([[DIFF_DELETE, 'a'], [DIFF_INSERT, 'b']], dmp.diff_main('a', 'b', false));

//   assertEquivalent([[DIFF_DELETE, 'Apple'], [DIFF_INSERT, 'Banana'], [DIFF_EQUAL, 's are a'], [DIFF_INSERT, 'lso'], [DIFF_EQUAL, ' fruit.']], dmp.diff_main('Apples are a fruit.', 'Bananas are also fruit.', false));

//   assertEquivalent([[DIFF_DELETE, 'a'], [DIFF_INSERT, '\u0680'], [DIFF_EQUAL, 'x'], [DIFF_DELETE, '\t'], [DIFF_INSERT, '\0']], dmp.diff_main('ax\t', '\u0680x\0', false));

//   // Overlaps.
//   assertEquivalent([[DIFF_DELETE, '1'], [DIFF_EQUAL, 'a'], [DIFF_DELETE, 'y'], [DIFF_EQUAL, 'b'], [DIFF_DELETE, '2'], [DIFF_INSERT, 'xab']], dmp.diff_main('1ayb2', 'abxab', false));

//   assertEquivalent([[DIFF_INSERT, 'xaxcx'], [DIFF_EQUAL, 'abc'], [DIFF_DELETE, 'y']], dmp.diff_main('abcy', 'xaxcxabc', false));

//   assertEquivalent([[DIFF_DELETE, 'ABCD'], [DIFF_EQUAL, 'a'], [DIFF_DELETE, '='], [DIFF_INSERT, '-'], [DIFF_EQUAL, 'bcd'], [DIFF_DELETE, '='], [DIFF_INSERT, '-'], [DIFF_EQUAL, 'efghijklmnopqrs'], [DIFF_DELETE, 'EFGHIJKLMNOefg']], dmp.diff_main('ABCDa=bcd=efghijklmnopqrsEFGHIJKLMNOefg', 'a-bcd-efghijklmnopqrs', false));

//   // Large equality.
//   assertEquivalent([[DIFF_INSERT, ' '], [DIFF_EQUAL, 'a'], [DIFF_INSERT, 'nd'], [DIFF_EQUAL, ' [[Pennsylvania]]'], [DIFF_DELETE, ' and [[New']], dmp.diff_main('a [[Pennsylvania]] and [[New', ' and [[Pennsylvania]]', false));

//   // Timeout.
//   dmp.Diff_Timeout = 0.1;  // 100ms
//   var a = '`Twas brillig, and the slithy toves\nDid gyre and gimble in the wabe:\nAll mimsy were the borogoves,\nAnd the mome raths outgrabe.\n';
//   var b = 'I am the very model of a modern major general,\nI\'ve information vegetable, animal, and mineral,\nI know the kings of England, and I quote the fights historical,\nFrom Marathon to Waterloo, in order categorical.\n';
//   // Increase the text lengths by 1024 times to ensure a timeout.
//   for (var i = 0; i < 10; i++) {
//     a += a;
//     b += b;
//   }
//   var startTime = (new Date()).getTime();
//   dmp.diff_main(a, b);
//   var endTime = (new Date()).getTime();
//   // Test that we took at least the timeout period.
//   assertTrue(dmp.Diff_Timeout * 1000 <= endTime - startTime);
//   // Test that we didn't take forever (be forgiving).
//   // Theoretically this test could fail very occasionally if the
//   // OS task swaps or locks up for a second at the wrong moment.
//   assertTrue(dmp.Diff_Timeout * 1000 * 2 > endTime - startTime);
//   dmp.Diff_Timeout = 0;

//   // Test the linemode speedup.
//   // Must be long to pass the 100 char cutoff.
//   // Simple line-mode.
//   a = '1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n';
//   b = 'abcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\n';
//   assertEquivalent(dmp.diff_main(a, b, false), dmp.diff_main(a, b, true));

//   // Single line-mode.
//   a = '1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890';
//   b = 'abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghij';
//   assertEquivalent(dmp.diff_main(a, b, false), dmp.diff_main(a, b, true));

//   // Overlap line-mode.
//   a = '1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n';
//   b = 'abcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n';
//   var texts_linemode = diff_rebuildtexts(dmp.diff_main(a, b, true));
//   var texts_textmode = diff_rebuildtexts(dmp.diff_main(a, b, false));
//   assertEquivalent(texts_textmode, texts_linemode);

//   // Test null inputs.
//   try {
//     dmp.diff_main(null, null);
//     assertEquals(Error, null);
//   } catch (e) {
//     // Exception expected.
//   }
    }
}