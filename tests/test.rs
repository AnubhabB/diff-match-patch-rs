use std::{collections::HashMap, time::Instant};

use chrono::Utc;

use diff_match_patch_rs::dmp::Diff;

use diff_match_patch_rs::{DiffMatchPatch, Error, Ops, Patch, PatchInput};

// const tests = [
//     'testDiffIsDestructurable',
//     'testDiffCleanupEfficiency',
// ];

#[test]
fn test_diff_levenshtein() {
    let dmp = DiffMatchPatch::new();
    let diffs = vec![
        Diff::delete(b"abc"),
        Diff::insert(b"1234"),
        Diff::equal(b"xyz"),
    ];
    assert_eq!(4, dmp.diff_levenshtein(&diffs));

    let diffs = vec![
        Diff::equal(b"xyz"),
        Diff::delete(b"abc"),
        Diff::insert(b"1234"),
    ];
    assert_eq!(4, dmp.diff_levenshtein(&diffs));

    let diffs = vec![
        Diff::delete(b"abc"),
        Diff::equal(b"xyz"),
        Diff::insert(b"1234"),
    ];
    assert_eq!(7, dmp.diff_levenshtein(&diffs));
}

#[test]
fn test_diff_bisect() -> Result<(), Error> {
    let mut dmp = DiffMatchPatch::default();

    // Normal.
    // Since the resulting diff hasn't been normalized, it would be ok if
    // the insertion and deletion pairs are swapped.
    // If the order changes, tweak this test as required.
    assert_eq!(
        vec![
            Diff::delete(b"c"),
            Diff::insert(b"m"),
            Diff::equal(b"a"),
            Diff::delete(b"t"),
            Diff::insert(b"p")
        ],
        dmp.bisect(b"cat", b"map", None)?
    );

    // Timeout.
    dmp.set_timeout(Some(0));
    let deadline = dmp.deadline();
    assert_eq!(
        vec![Diff::delete(b"cat"), Diff::insert(b"map"),],
        dmp.bisect(b"cat", b"map", deadline)?
    );

    Ok(())
}

#[test]
fn test_diff_pretty_html() -> Result<(), Error> {
    let dmp = DiffMatchPatch::new();
    // Basic
    let diffs = [
        Diff::equal(b"a\n"),
        Diff::delete(b"<B>b</B>"),
        Diff::insert(b"c&d"),
    ];
    assert_eq!("<span>a&para;<br></span><del style=\"background:#ffe6e6;\">&lt;B&gt;b&lt;/B&gt;</del><ins style=\"background:#e6ffe6;\">c&amp;d</ins>", dmp.diff_pretty_html(&diffs)?);

    // Monkey busiess around Emoticons and extended utf-8 🤪🤩🤔
    // This gave me a lot of heart-burn

    // Case 1. Two similar emoticons
    // In bytes representation, these would have the last u8 different
    // Which means the the diff should an equality block of 3 bytes folloed by insert and delete
    let old = "🤪"; // [240, 159, 164, 170]
    let new = "🤔"; // [240, 159, 164, 148]
    let diffs = dmp.diff_main(old, new)?;
    assert_eq!(
        "<span></span><del style=\"background:#ffe6e6;\">🤪</del><ins style=\"background:#e6ffe6;\">🤔</ins>",
        dmp.diff_pretty_html(&diffs)?
    );

    // Now Case 1. but with some text before and after
    let old = "I'm puzzled🤪 or am I?";
    let new = "I'm puzzled🤔 or thinking I guess!";
    let diffs = dmp.diff_main(old, new)?;
    assert_eq!(
        "<span>I'm puzzled</span><del style=\"background:#ffe6e6;\">🤪</del><ins style=\"background:#e6ffe6;\">🤔</ins><span> or </span><del style=\"background:#ffe6e6;\">am I?</del><ins style=\"background:#e6ffe6;\">thinking I guess!</ins>",
        dmp.diff_pretty_html(&diffs)?
    );

    // Case 2. Emoticons with the third position different
    let old = "🍊"; // [240, 159, 141, 138]
    let new = "🌊"; // [240, 159, 140, 138]
    let diffs = dmp.diff_main(old, new)?;
    assert_eq!(
        "<span></span><del style=\"background:#ffe6e6;\">🍊</del><ins style=\"background:#e6ffe6;\">🌊</ins>",
        dmp.diff_pretty_html(&diffs)?
    );

    // Now Case 2. but with some text, lets complicate this
    let old = "🍊, aah orange is the new black!"; // [240, 159, 141, 138]
    let new = "Aah orange!🌊is the new 🌊"; // [240, 159, 140, 138]
    let diffs = dmp.diff_main(old, new)?;
    assert_eq!(
        "<del style=\"background:#ffe6e6;\">🍊, a</del><ins style=\"background:#e6ffe6;\">A</ins><span>ah orange</span><del style=\"background:#ffe6e6;\"> </del><ins style=\"background:#e6ffe6;\">!🌊</ins><span>is the new </span><del style=\"background:#ffe6e6;\">black!</del><ins style=\"background:#e6ffe6;\">🌊</ins>",
        dmp.diff_pretty_html(&diffs)?
    );

    // Case 3. with second and third different, but lets complicate this with an equality
    let old = "𠌊"; // [240, 160, 140, 138]
    let new = "𖠊"; // [240, 150, 160, 138]
    let diffs = dmp.diff_main(old, new)?;
    assert_eq!(
        "<span></span><ins style=\"background:#e6ffe6;\">𖠊</ins><del style=\"background:#ffe6e6;\">𠌊</del>",
        dmp.diff_pretty_html(&diffs)?
    );

    // Case 3. but let there be a swap
    let old = "𞠄"; // [240, 158, 160, 132]
    let new = std::str::from_utf8(&[240, 160, 158, 132]).unwrap(); // basically an undefined element `𠞄`. Should still work
    let diffs = dmp.diff_main(old, new)?;
    assert_eq!(
        "<span></span><del style=\"background:#ffe6e6;\">𞠄</del><ins style=\"background:#e6ffe6;\">𠞄</ins>",
        dmp.diff_pretty_html(&diffs)?
    );

    // Case 4. swap at the last 2 positions
    let old = "🍌"; // [240, 159, 141, 140] -- FINALLY A BANANA
    let new = "🌍"; // [240, 159, 140, 141] -- interesting revelation - last 2 bytes swapped and 🍌 becomes 🌍. Guess the world is going `Bananas!!`
    let diffs = dmp.diff_main(old, new)?;
    assert_eq!(
        "<span></span><del style=\"background:#ffe6e6;\">🍌</del><ins style=\"background:#e6ffe6;\">🌍</ins>",
        dmp.diff_pretty_html(&diffs)?
    );

    // Let's do this with a slightly longish string
    let old = "Now, let's explore some emotional extremes 🌊.\nWe've got your ecstatic face 🤩, your devastated face 😭, and your utterly confused face 🤯. But that's not all! 🤔 We've also got some subtle emotions like 😐, 🙃, and 👀.";
    let new = "Let's start with some basics 😊.\nWe've got your standard smiley face 🙂, your sad face ☹️, and your angry face 😠. But wait, there's more! 🤩 We've also got some more complex emotions like 😍, 🤤, and 🚀. And let's not forget about the classics: 😉, 👍, and 👏.";
    let diffs = dmp.diff_main(old, new)?;

    assert_eq!(
        "<del style=\"background:#ffe6e6;\">Now, let's explore some emotional extreme</del><ins style=\"background:#e6ffe6;\">Let's start with some basic</ins><span>s </span><del style=\"background:#ffe6e6;\">🌊</del><ins style=\"background:#e6ffe6;\">😊</ins><span>.&para;<br>We've got your </span><del style=\"background:#ffe6e6;\">ec</del><span>sta</span><del style=\"background:#ffe6e6;\">tic</del><ins style=\"background:#e6ffe6;\">ndard smiley</ins><span> face </span><del style=\"background:#ffe6e6;\">🤩</del><ins style=\"background:#e6ffe6;\">🙂</ins><span>, your </span><del style=\"background:#ffe6e6;\">devastate</del><ins style=\"background:#e6ffe6;\">sa</ins><span>d face </span><del style=\"background:#ffe6e6;\">😭</del><ins style=\"background:#e6ffe6;\">☹️</ins><span>, and your </span><del style=\"background:#ffe6e6;\">utterly confused</del><ins style=\"background:#e6ffe6;\">angry</ins><span> face </span><del style=\"background:#ffe6e6;\">🤯</del><ins style=\"background:#e6ffe6;\">😠</ins><span>. But </span><del style=\"background:#ffe6e6;\">that's not all</del><ins style=\"background:#e6ffe6;\">wait, there's more</ins><span>! </span><del style=\"background:#ffe6e6;\">🤔</del><ins style=\"background:#e6ffe6;\">🤩</ins><span> We've also got some </span><del style=\"background:#ffe6e6;\">subt</del><ins style=\"background:#e6ffe6;\">more comp</ins><span>le</span><ins style=\"background:#e6ffe6;\">x</ins><span> emotions like </span><del style=\"background:#ffe6e6;\">😐</del><ins style=\"background:#e6ffe6;\">😍, 🤤, and 🚀. And let's not forget about the classics: 😉</ins><span>, </span><del style=\"background:#ffe6e6;\">🙃</del><ins style=\"background:#e6ffe6;\">👍</ins><span>, and </span><del style=\"background:#ffe6e6;\">👀</del><ins style=\"background:#e6ffe6;\">👏</ins><span>.</span>",
        dmp.diff_pretty_html(&diffs)?
    );

    Ok(())
}

#[test]
fn test_diff_main() -> Result<(), Error> {
    let mut dmp = DiffMatchPatch::default();

    // Perform a trivial diff.
    // Null case.
    assert!(dmp.diff_main("", "")?.is_empty());

    // Equality
    assert_eq!(vec![Diff::equal(b"abc")], dmp.diff_main("abc", "abc")?);

    // Simple insert
    assert_eq!(
        vec![Diff::equal(b"ab"), Diff::insert(b"123"), Diff::equal(b"c")],
        dmp.diff_main("abc", "ab123c")?
    );

    // Simple delete
    assert_eq!(
        vec![Diff::equal(b"a"), Diff::delete(b"123"), Diff::equal(b"bc")],
        dmp.diff_main("a123bc", "abc")?
    );

    // Two insertions
    assert_eq!(
        vec![
            Diff::equal(b"a"),
            Diff::insert(b"123"),
            Diff::equal(b"b"),
            Diff::insert(b"456"),
            Diff::equal(b"c"),
        ],
        dmp.diff_main("abc", "a123b456c")?
    );

    // Two deletions.
    assert_eq!(
        vec![
            Diff::equal(b"a"),
            Diff::delete(b"123"),
            Diff::equal(b"b"),
            Diff::delete(b"456"),
            Diff::equal(b"c"),
        ],
        dmp.diff_main("a123b456c", "abc")?
    );

    // Perform a real diff.
    // Switch off the timeout.
    dmp.set_timeout(None);
    // Simple cases.
    assert_eq!(
        vec![Diff::delete(b"a"), Diff::insert(b"b"),],
        dmp.diff_main("a", "b")?
    );

    assert_eq!(
        vec![
            Diff::delete(b"Apple"),
            Diff::insert(b"Banana"),
            Diff::equal(b"s are a"),
            Diff::insert(b"lso"),
            Diff::equal(b" fruit.")
        ],
        dmp.diff_main("Apples are a fruit.", "Bananas are also fruit.")?
    );

    assert_eq!(
        vec![
            Diff::delete(b"a"),
            Diff::insert("\u{0680}".as_bytes()),
            Diff::equal(b"x"),
            Diff::delete(b"\t"),
            Diff::insert(b"\0")
        ],
        dmp.diff_main("ax\t", "\u{0680}x\0")?
    );

    // Overlaps.
    assert_eq!(
        vec![
            Diff::delete(b"1"),
            Diff::equal(b"a"),
            Diff::delete(b"y"),
            Diff::equal(b"b"),
            Diff::delete(b"2"),
            Diff::insert(b"xab"),
        ],
        dmp.diff_main("1ayb2", "abxab")?
    );

    assert_eq!(
        vec![
            Diff::insert(b"xaxcx"),
            Diff::equal(b"abc"),
            Diff::delete(b"y"),
        ],
        dmp.diff_main("abcy", "xaxcxabc")?
    );

    assert_eq!(
        vec![
            Diff::delete(b"ABCD"),
            Diff::equal(b"a"),
            Diff::delete(b"="),
            Diff::insert(b"-"),
            Diff::equal(b"bcd"),
            Diff::delete(b"="),
            Diff::insert(b"-"),
            Diff::equal(b"efghijklmnopqrs"),
            Diff::delete(b"EFGHIJKLMNOefg"),
        ],
        dmp.diff_main(
            "ABCDa=bcd=efghijklmnopqrsEFGHIJKLMNOefg",
            "a-bcd-efghijklmnopqrs"
        )?
    );

    // Large equality.
    assert_eq!(
        vec![
            Diff::insert(b" "),
            Diff::equal(b"a"),
            Diff::insert(b"nd"),
            Diff::equal(b" [[Hepatopancreatic]]"),
            Diff::delete(b" and [[New"),
        ],
        dmp.diff_main(
            "a [[Hepatopancreatic]] and [[New",
            " and [[Hepatopancreatic]]"
        )?
    );

    // Timeout.
    const LOW_TIMEOUT: u32 = 100;
    dmp.set_timeout(Some(LOW_TIMEOUT));
    let a = vec!["`Twas brillig, and the slithy toves\nDid gyre and gimble in the wabe:\nAll mimsy were the borogoves,\nAnd the mome raths outgrabe.\n"; 2048].join("");
    let b = vec!["I am the very model of a modern major general,\nI\'ve information vegetable, animal, and mineral,\nI know the kings of England, and I quote the fights historical,\nFrom Marathon to Waterloo, in order categorical.\n"; 2048].join("");

    let start = Utc::now().time();
    dmp.diff_main(&a, &b)?;
    let end = Utc::now().time();
    // Test that we took at least the timeout period (+ 5ms being generous).
    assert!((end - start).num_milliseconds() <= LOW_TIMEOUT as i64 + 5);

    // Test the linemode speedup.
    // Must be long to pass the 100 char cutoff.
    // Simple line-mode.
    dmp.set_timeout(Some(1000));
    let a =  "12345678901234567890123456789 0123456 78901234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n";
    let b = "abcdefghij abcdefghij abcdefghij abcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\n";
    dmp.set_checklines(false);
    let res_no_lm = dmp.diff_main(a, b)?;
    dmp.set_checklines(true);
    let res_yes_lm = dmp.diff_main(a, b)?;

    // Now, we'll run 2 checks - one for result equality
    assert_eq!(res_no_lm, res_yes_lm);

    // Single line-mode.
    let a = "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";
    let b = "abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghij";
    dmp.set_checklines(false);
    let yes_lm = dmp.diff_main(a, b)?;
    dmp.set_checklines(true);
    let no_lm = dmp.diff_main(a, b)?;
    assert_eq!(no_lm, yes_lm);

    // Overlap line-mode.
    let a = "1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n";
    let b = "abcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n";
    dmp.set_checklines(false);
    let no_lm = dmp.diff_main(a, b)?;
    dmp.set_checklines(true);
    let yes_lm = dmp.diff_main(a, b)?;
    assert_eq!(rebuild_text(&yes_lm[..])?, rebuild_text(&no_lm[..])?);

    // Benefits of checklines can only be realized in text with many lines
    let mut dmp = DiffMatchPatch::default();
    let old = std::fs::read_to_string("testdata/txt_old.txt").unwrap();
    let new = std::fs::read_to_string("testdata/txt_new.txt").unwrap();
    
    let start = Instant::now();
    let diff_yes_lm = dmp.diff_main(&old, &new);
    let yes_lm_dur = Instant::now() - start;
    assert!(diff_yes_lm.is_ok());

    dmp.set_checklines(false);
    let start = Instant::now();
    let diff_no_lm = dmp.diff_main(&old, &new);
    let no_lm_dur = Instant::now() - start;
    assert!(diff_no_lm.is_ok());
    
    assert!(no_lm_dur > yes_lm_dur);
    Ok(())
}

#[test]
fn test_diff_delta() -> Result<(), Error> {
    let diffs = vec![
        Diff::equal(b"jump"),
        Diff::delete(b"s"),
        Diff::insert(b"ed"),
        Diff::equal(b" over "),
        Diff::delete(b"the"),
        Diff::insert(b"a"),
        Diff::equal(b" lazy"),
        Diff::insert(b"old dog"),
    ];
    let txt_old = "jumps over the lazy".as_bytes();
    assert_eq!(
        txt_old,
        DiffMatchPatch::diff_text_old(&diffs)
    );

    let delta = DiffMatchPatch::to_delta(&diffs);
    assert_eq!("=4\t-1\t+ed\t=6\t-3\t+a\t=5\t+old dog".as_bytes(), &delta);
    // Convert delta string into a diff.
    assert_eq!(diffs, DiffMatchPatch::from_delta(txt_old, &delta)?);
    
    // Generates error (19 != 20).
    assert!(DiffMatchPatch::from_delta(&[txt_old, "+".as_bytes()].concat()[..], &delta).is_err());
    
    // Generates error (19 != 18).
    assert!(DiffMatchPatch::from_delta(&txt_old[1..], &delta).is_err());
    
    // Test deltas with special characters.
    let diffs = vec![
        Diff::equal("\u{0680} \x00 \t %".as_bytes()),
        Diff::delete("\u{0681} \x01 \n ^".as_bytes()),
        Diff::insert("\u{0682} \x02 \\ |".as_bytes())
    ];
    let txt_old = DiffMatchPatch::diff_text_old(&diffs);
    assert_eq!("\u{0680} \x00 \t %\u{0681} \x01 \n ^".as_bytes(), txt_old);
    let delta = DiffMatchPatch::to_delta(&diffs);

    assert_eq!(b"=8\t-8\t+%DA%82 %02 %5C %7C", &delta[..]);
    // Convert delta string into a diff.
    assert_eq!(&diffs, &DiffMatchPatch::from_delta(&txt_old, &delta)?);

    // Verify pool of unchanged characters.
    let diffs = vec![
        Diff::insert("A-Z a-z 0-9 - _ . ! ~ * ' ( ) ; / ? : @ & = + $ , # ".as_bytes())
    ];
    let txt_new = DiffMatchPatch::diff_text_new(&diffs);
    assert_eq!("A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # ", std::str::from_utf8(&txt_new).unwrap());

    let delta = DiffMatchPatch::to_delta(&diffs);
    assert_eq!("+A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # ", std::str::from_utf8(&delta).unwrap());

    // Convert delta string into a diff.
    assert_eq!(diffs, DiffMatchPatch::from_delta("".as_bytes(), &delta)?);
    Ok(())
}

// Helper to construct the two texts which made up the diff originally.
fn rebuild_text(diffs: &[Diff<u8>]) -> Result<(String, String), Error> {
    let mut txt1 = vec![];
    let mut txt2 = vec![];

    diffs.iter().for_each(|d| {
        if d.op() != Ops::Insert {
            txt1.push(d.data());
        }

        if d.op() != Ops::Delete {
            txt2.push(d.data());
        }
    });

    Ok((
        String::from_utf8(txt1.concat()).map_err(|_| Error::Utf8Error)?,
        String::from_utf8(txt2.concat()).map_err(|_| Error::Utf8Error)?,
    ))
}

#[test]
fn test_patch_from_text() -> Result<(), Error> {
    let dmp = DiffMatchPatch::new();

    assert!(dmp.patch_from_text("")?.is_empty());

    let strp = "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n";
    assert_eq!(
        strp,
        dmp.patch_from_text(
            strp
        )?[0].to_string()
    );

    assert_eq!(
        "@@ -1 +1 @@\n-a\n+b\n",
        dmp.patch_from_text("@@ -1 +1 @@\n-a\n+b\n")?[0].to_string()
    );

    assert_eq!(
        "@@ -1,3 +0,0 @@\n-abc\n",
        dmp.patch_from_text("@@ -1,3 +0,0 @@\n-abc\n")?[0].to_string()
    );

    assert_eq!(
        "@@ -0,0 +1,3 @@\n+abc\n",
        dmp.patch_from_text("@@ -0,0 +1,3 @@\n+abc\n")?[0].to_string()
    );

    // Generates error.
    assert!(dmp.patch_from_text("Bad\nPatch\n").is_err());

    Ok(())
}

#[test]
fn test_patch_to_text() -> Result<(), Error> {
    let dmp = DiffMatchPatch::new();

    let strp = "@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n";
    let patches = dmp.patch_from_text(strp)?;
    assert_eq!(strp, dmp.patch_to_text(&patches));

    let strp = "@@ -1,9 +1,9 @@\n-f\n+F\n oo+fooba\n@@ -7,9 +7,9 @@\n obar\n-,\n+.\n  tes\n";
    let patches = dmp.patch_from_text(strp)?;
    assert_eq!(strp, dmp.patch_to_text(&patches));

    Ok(())
}

#[test]
fn test_patch_make() -> Result<(), Error> {
    let dmp = DiffMatchPatch::default();
    let patches = dmp.patch_make(PatchInput::Texts("", ""))?;
    assert!(patches.is_empty());

    let txt1 = "The quick brown fox jumps over the lazy dog.";
    let txt2 = "That quick brown fox jumped over a lazy dog.";

    // The second patch must be "-21,17 +21,18", not "-22,17 +21,18" due to rolling context.
    let patches = dmp.patch_make(PatchInput::Texts(txt2, txt1))?;
    assert_eq!("@@ -1,8 +1,7 @@\n Th\n-at\n+e\n  qui\n@@ -21,17 +21,18 @@\n jump\n-ed\n+s\n  over \n-a\n+the\n  laz\n", dmp.patch_to_text(&patches));

    // Text1+Text2 inputs.
    let patches = dmp.patch_make(PatchInput::Texts(txt1, txt2))?;
    assert_eq!("@@ -1,11 +1,12 @@\n Th\n-e\n+at\n  quick b\n@@ -22,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n", dmp.patch_to_text(&patches));

    // Diff input.
    let diffs = dmp.diff_main(txt1, txt2)?;
    let patches = dmp.patch_make(PatchInput::Diffs(&diffs[..]))?;
    assert_eq!("@@ -1,11 +1,12 @@\n Th\n-e\n+at\n  quick b\n@@ -22,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n", dmp.patch_to_text(&patches));

    // Text1+Diff inputs.
    let patches = dmp.patch_make(PatchInput::TextDiffs(txt1, &diffs[..]))?;
    assert_eq!("@@ -1,11 +1,12 @@\n Th\n-e\n+at\n  quick b\n@@ -22,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n", dmp.patch_to_text(&patches));

    // Character encoding.
    let patches = dmp.patch_make(PatchInput::Texts(
        "`1234567890-=[]\\;',./",
        "~!@#$%^&*()_+{}|:\"<>?",
    ))?;
    
    assert_eq!(
        "@@ -1,21 +1,21 @@\n-%601234567890-=%5B%5D%5C;',./\n+~!@#$%25%5E&*()_+%7B%7D%7C:%22%3C%3E?\n",
        dmp.patch_to_text(&patches)
    );

    // Character decoding.
    let diffs = vec![
        Diff::delete(b"`1234567890-=[]\\;',./"),
        Diff::insert(b"~!@#$%^&*()_+{}|:\"<>?"),
    ];
    assert_eq!(
        diffs,
        dmp.patch_from_text("@@ -1,21 +1,21 @@\n-%601234567890-=%5B%5D%5C;',./\n+~!@#$%25%5E&*()_+%7B%7D%7C:%22%3C%3E?\n")?[0].diffs()
    );

    // Long string with repeats.
    let txt1 = vec!["abcdef"; 100].join("");
    let txt2 = [&txt1, "123"].join("");
    let patches = dmp.patch_make(PatchInput::Texts(&txt1, &txt2))?;
    assert_eq!(
        "@@ -573,28 +573,31 @@\n cdefabcdefabcdefabcdefabcdef\n+123\n",
        dmp.patch_to_text(&patches)
    );

    Ok(())
}

#[test]
fn test_diff_text() {
    let diffs = vec![
        Diff::equal(b"jump"),
        Diff::delete(b"s"),
        Diff::insert(b"ed"),
        Diff::equal(b" over "),
        Diff::delete(b"the"),
        Diff::insert(b"a"),
        Diff::equal(b" lazy"),
    ];

    assert_eq!(
        b"jumps over the lazy",
        &DiffMatchPatch::diff_text_old(&diffs[..])[..]
    );
    assert_eq!(
        b"jumped over a lazy",
        &DiffMatchPatch::diff_text_new(&diffs[..])[..]
    );
}


#[test]
fn test_patch_apply() -> Result<(), Error> {
    let mut dmp = DiffMatchPatch::default();

    let patches = dmp.patch_make(PatchInput::Texts("", ""))?;
    let (txt, results) = dmp.patch_apply_internal(&patches, b"Hello world.")?;
    assert_eq!(
        format!("{}\t{}", std::str::from_utf8(&txt).unwrap(), results.len()),
        "Hello world.\t0"
    );

    let patches = dmp.patch_make(PatchInput::Texts(
        "The quick brown fox jumps over the lazy dog.",
        "That quick brown fox jumped over a lazy dog.",
    ))?;

    // Exact match
    assert_eq!(
        (
            b"That quick brown fox jumped over a lazy dog.".to_vec(),
            vec![true, true]
        ),
        dmp.patch_apply_internal(&patches, b"The quick brown fox jumps over the lazy dog.")?
    );

    // Partial match
    assert_eq!(
        (
            b"That quick red rabbit jumped over a tired tiger.".to_vec(),
            vec![true, true]
        ),
        dmp.patch_apply_internal(
            &patches,
            b"The quick red rabbit jumps over the tired tiger."
        )?
    );

    // Failed match
    assert_eq!(
        (
            b"I am the very model of a modern major general.".to_vec(),
            vec![false, false]
        ),
        dmp.patch_apply_internal(&patches, b"I am the very model of a modern major general.")?
    );

    // Big delete, small change
    let patches = dmp.patch_make(PatchInput::Texts(
        "x1234567890123456789012345678901234567890123456789012345678901234567890y",
        "xabcy",
    ))?;
    assert_eq!((b"xabcy".to_vec(), vec![true, true]), dmp.patch_apply_internal(&patches, b"x123456789012345678901234567890-----++++++++++-----123456789012345678901234567890y")?);

    // Big delete, large change
    let patches = dmp.patch_make(PatchInput::Texts(
        "x1234567890123456789012345678901234567890123456789012345678901234567890y",
        "xabcy",
    ))?;
    assert_eq!((b"xabc12345678901234567890---------------++++++++++---------------12345678901234567890y".to_vec(), vec![false, true]), dmp.patch_apply_internal(&patches, b"x12345678901234567890---------------++++++++++---------------12345678901234567890y")?);

    dmp.delete_threshold = 0.6;
    let patches = dmp.patch_make(PatchInput::Texts(
        "x1234567890123456789012345678901234567890123456789012345678901234567890y",
        "xabcy",
    ))?;
    assert_eq!((b"xabcy".to_vec(), vec![true, true]), dmp.patch_apply_internal(&patches, b"x12345678901234567890---------------++++++++++---------------12345678901234567890y")?);
    dmp.delete_threshold = 0.5;

    // Compesate for failed patch
    dmp.match_threshold = 0.;
    dmp.match_distance = 0;
    let patches = dmp.patch_make(PatchInput::Texts(
        "abcdefghijklmnopqrstuvwxyz--------------------1234567890",
        "abcXXXXXXXXXXdefghijklmnopqrstuvwxyz--------------------1234567YYYYYYYYYY890",
    ))?;
    assert_eq!(
        (
            b"ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567YYYYYYYYYY890".to_vec(),
            vec![false, true]
        ),
        dmp.patch_apply_internal(
            &patches,
            b"ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567890"
        )?
    );
    dmp.match_threshold = 0.5;
    dmp.match_distance = 1000;

    // No side-effects - kinds useless cos patches is not mutable in rust
    let patches = dmp.patch_make(PatchInput::Texts("", "test"))?;
    let srcstr = dmp.patch_to_text(&patches);
    dmp.patch_apply_internal(&patches, b"")?;
    assert_eq!(srcstr, dmp.patch_to_text(&patches));

    let patches = dmp.patch_make(PatchInput::Texts(
        "The quick brown fox jumps over the lazy dog.",
        "Woof",
    ))?;
    let srcstr = dmp.patch_to_text(&patches);
    dmp.patch_apply_internal(&patches, b"The quick brown fox jumps over the lazy dog.")?;
    assert_eq!(srcstr, dmp.patch_to_text(&patches));

    // Edge exact match
    let patches = dmp.patch_make(PatchInput::Texts("", "test"))?;
    assert_eq!(
        (b"test".to_vec(), vec![true]),
        dmp.patch_apply_internal(&patches, b"")?
    );

    // Near edge exact match
    let patches = dmp.patch_make(PatchInput::Texts("XY", "XtestY"))?;
    assert_eq!(
        (b"XtestY".to_vec(), vec![true]),
        dmp.patch_apply_internal(&patches, b"XY")?
    );

    // Edge partial match
    let patches = dmp.patch_make(PatchInput::Texts("y", "y123"))?;
    assert_eq!(
        (b"x123".to_vec(), vec![true]),
        dmp.patch_apply_internal(&patches, b"x")?
    );

    Ok(())
}

#[test]
fn test_match_alphabet() {
    // Initialise the bitmasks for Bitap.
    // Unique.
    assert_eq!(
        HashMap::from([(b'a', 4), (b'b', 2), (b'c', 1)]),
        DiffMatchPatch::match_alphabet(b"abc")
    );

    // Duplicates.
    assert_eq!(
        HashMap::from([(b'a', 37), (b'b', 18), (b'c', 8)]),
        DiffMatchPatch::match_alphabet(b"abcaba")
    )
}

#[test]
fn test_match_bitap() {
    // Bitap algorithm.
    let mut dmp = DiffMatchPatch {
        match_distance: 100,
        ..Default::default()
    };

    // Exact matches.
    assert_eq!(Some(5), dmp.match_bitap(b"abcdefghijk", b"fgh", 5));
    assert_eq!(Some(5), dmp.match_bitap(b"abcdefghijk", b"fgh", 0));

    // Fuzzy matches.
    assert_eq!(Some(4), dmp.match_bitap(b"abcdefghijk", b"efxhi", 0));
    assert_eq!(Some(2), dmp.match_bitap(b"abcdefghijk", b"cdefxyhijk", 5));
    assert_eq!(None, dmp.match_bitap(b"abcdefghijk", b"bxy", 1));

    // Overflow.
    assert_eq!(Some(2), dmp.match_bitap(b"123456789xx0", b"3456789x0", 2));

    // Threshold test.
    dmp.match_threshold = 0.4;
    assert_eq!(Some(4), dmp.match_bitap(b"abcdefghijk", b"efxyhi", 1));

    // dmp.`match_threshold` = 0.3;
    dmp.match_threshold = 0.3;
    assert_eq!(None, dmp.match_bitap(b"abcdefghijk", b"efxyhi", 1));

    dmp.match_threshold = 0.;
    assert_eq!(Some(1), dmp.match_bitap(b"abcdefghijk", b"bcdef", 1));

    dmp.match_threshold = 0.5;

    // Multiple select.
    assert_eq!(Some(0), dmp.match_bitap(b"abcdexyzabcde", b"abccde", 3));
    assert_eq!(Some(8), dmp.match_bitap(b"abcdexyzabcde", b"abccde", 5));

    // Distance test.
    dmp.match_distance = 10;
    assert_eq!(
        None,
        dmp.match_bitap(b"abcdefghijklmnopqrstuvwxyz", b"abcdefg", 24)
    );
    assert_eq!(
        Some(0),
        dmp.match_bitap(b"abcdefghijklmnopqrstuvwxyz", b"abcdxxefg", 1)
    );

    dmp.match_distance = 1000;
    assert_eq!(
        Some(0),
        dmp.match_bitap(b"abcdefghijklmnopqrstuvwxyz", b"abcdefg", 24)
    );
}

#[test]
fn test_match_main() {
    let dmp = DiffMatchPatch::default();
    // Full match.
    // Shortcut matches.
    assert_eq!(Some(0), dmp.match_internal(b"abcdef", b"abcdef", 1000));
    assert_eq!(None, dmp.match_internal(b"", b"abcdef", 1));
    assert_eq!(Some(3), dmp.match_internal(b"abcdef", b"", 3));
    assert_eq!(Some(3), dmp.match_internal(b"abcdef", b"de", 3));

    // Beyond end match.
    assert_eq!(Some(3), dmp.match_internal(b"abcdef", b"defy", 4));

    // Oversized pattern.
    assert_eq!(Some(0), dmp.match_internal(b"abcdef", b"abcdefy", 0));

    // Complex match.
    assert_eq!(
        Some(4),
        dmp.match_internal(
            b"I am the very model of a modern major general.",
            b" that berry ",
            5
        )
    );
}