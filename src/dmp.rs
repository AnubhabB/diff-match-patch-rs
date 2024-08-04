use std::{time::{Duration, Instant}, u32};


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

impl DiffMatchPatch {
    fn checklines(&self) -> bool {
        self.checklines.map_or(true, |c| c)
    }

    // returns the configured timeout, defaults to `1`, None or `0` would mean infinite timeout
    fn timeout(&self) -> u64 {
        self.timeout.map_or(u64::MAX, |tout| if tout >  0 { tout } else { u64::MAX })
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

        let idx = long.windows(short.len()).step_by(1).position(|k| k == short);
        // found a subsequence which contains the short text
        if let Some(idx) = idx {
            // Shorter text is inside the longer text (speedup).
            let op = if old_gt_new { Ops::Delete } else { Ops::Insert };
            let diffs = vec![
                Diff::new(op, &long[0 .. idx]),
                Diff::new(Ops::Equal, short),
                Diff::new(op, &long[idx .. short.len()])
            ];

            return diffs;
        }

        if short.len() == 1 {
            // After previous case, this can't be an equality
            return vec![Diff::new(Ops::Delete, old), Diff::new(Ops::Insert, new)];
        }
        
        // Check if the problem can be split in two
        if let Some(half_match) = self.diff_half_match(old, new) {

        }
        //   // Check to see if the problem can be split in two.
        //   var hm = this.diff_halfMatch_(text1, text2);
        //   if (hm) {
        //     // A half-match was found, sort out the return data.
        //     var text1_a = hm[0];
        //     var text1_b = hm[1];
        //     var text2_a = hm[2];
        //     var text2_b = hm[3];
        //     var mid_common = hm[4];
        //     // Send both pairs off for separate processing.
        //     var diffs_a = this.diff_main(text1_a, text2_a, checklines, deadline);
        //     var diffs_b = this.diff_main(text1_b, text2_b, checklines, deadline);
        //     // Merge the results.
        //     return diffs_a.concat([new diff_match_patch.Diff(DIFF_EQUAL, mid_common)],
        //                           diffs_b);
        //   }
        
        //   if (checklines && text1.length > 100 && text2.length > 100) {
        //     return this.diff_lineMode_(text1, text2, deadline);
        //   }
        
        //   return this.diff_bisect_(text1, text2, deadline);

        todo!()
    }

    fn diff_half_match(&self, old: &[u8], new: &[u8]) -> Option<()> {
        // Don't risk returning a suboptimal diff when we have unlimited time
        if self.timeout() == u64::MAX {
            return None;
        }

        let (long, short, old_gt_new) = if old.len() > new.len() { (old, new, true) } else { (new, old, false) };
        // pointless - two small for this algo
        if long.len() < 4 || short.len() * 2 < long.len() {
            return None;
        }

        
        None
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
        // First, check if lhs and rhs are equal
        if old == new {
            if old.is_empty() {
                return Vec::new();
            }

            return vec![Diff::new(Ops::Equal, old.as_bytes())];
        }

        if old.is_empty() {
            return vec![Diff::new(Ops::Insert, new.as_bytes())]
        }

        if new.is_empty() {
            return vec![Diff::new(Ops::Delete, old.as_bytes())]
        }

        let deadline = Instant::now().checked_add(Duration::from_secs(self.timeout())).unwrap();

        let old_bytes = old.as_bytes();
        let new_bytes = new.as_bytes();

        // Trim common prefix
        let common_prefix = Self::common_prefix(old_bytes, new_bytes, false);
        let common_suffix = Self::common_prefix(&old_bytes[common_prefix..], &new_bytes[common_prefix..], true);

        let diffs = self.diff_compute(&old_bytes[common_prefix .. old_bytes.len() - common_suffix], &new_bytes[common_prefix .. new_bytes.len() - common_suffix]);

        todo!()
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
    use crate::dmp::{Diff, Ops};

    use super::DiffMatchPatch;

    // const tests = [
    //     'testDiffIsDestructurable', // TODO
    //     'testDiffCommonOverlap',
    //     'testDiffHalfMatch',
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