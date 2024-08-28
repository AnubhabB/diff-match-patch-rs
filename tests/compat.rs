use diff_match_patch_rs::{Compat, DiffMatchPatch, Error, PatchInput};

// Compatibility with other libraries

// Python Compat
// Check https://github.com/AnubhabB/diff-match-patch-rs-bench/python/compat.py
#[test]
fn test_compat_python_patch_text() -> Result<(), Error> {
    const TXT_OLD: &str = "** Paragraph 2
Let's start with some basics 😊. We've got your standard smiley face 🙂, your sad face ☹️, and your angry face 😠. But wait, there's more! 🤩 We've also got some more complex emotions like 😍, 🤤, and 🚀. And let's not forget about the classics: 😉, 👍, and 👏.";

    const TXT_NEW: &str = "** Paragraph 2
Now, let's explore some emotional extremes 🌊. We've got your ecstatic face 🤩, your devastated face 😭, and your utterly confused face 🤯. But that's not all! 🤔 We've also got some subtle emotions like 😐, 🙃, and 👀.";

    // Patch generated from python library [diff-match-patch](https://pypi.org/project/diff-match-patch/) for the above text
    const PATCH_TXT: &str = "@@ -12,38 +12,52 @@\n h 2%0A\n-Let's start with some basics %F0%9F%98%8A\n+Now, let's explore some emotional extremes %F0%9F%8C%8A\n . We\n@@ -73,48 +73,47 @@\n our \n+ec\n sta\n-ndard smiley\n+tic\n  face \n-%F0%9F%99%82\n+%F0%9F%A4%A9\n , your \n-sa\n+devastate\n d face \n-%E2%98%B9%EF%B8%8F\n+%F0%9F%98%AD\n , an\n@@ -123,47 +123,54 @@\n our \n-angry\n+utterly confused\n  face \n-%F0%9F%98%A0\n+%F0%9F%A4%AF\n . But \n-wait, there's more! %F0%9F%A4%A9\n+that's not all! %F0%9F%A4%94\n  We'\n@@ -190,20 +190,14 @@\n ome \n-more comp\n+subt\n le\n-x\n  emo\n@@ -211,70 +211,16 @@\n ike \n-%F0%9F%98%8D, %F0%9F%A4%A4\n+%F0%9F%98%90, %F0%9F%99%83\n , and \n-%F0%9F%9A%80. And let's not forget about the classics: %F0%9F%98%89, %F0%9F%91%8D, and %F0%9F%91%8F\n+%F0%9F%91%80\n .\n";

    let dmp = DiffMatchPatch::default();
    let patches = dmp.patch_from_text::<Compat>(PATCH_TXT)?;
    let (txt_new, _) = dmp.patch_apply(&patches, TXT_OLD)?;

    // Check if Python patch can be converted to new text
    assert_eq!(TXT_NEW, &txt_new);

    Ok(())
}

#[test]
fn test_compat_python_patch_delta() -> Result<(), Error> {
    const TXT_OLD: &str = "** Paragraph 2
Let's start with some basics 😊. We've got your standard smiley face 🙂, your sad face ☹️, and your angry face 😠. But wait, there's more! 🤩 We've also got some more complex emotions like 😍, 🤤, and 🚀. And let's not forget about the classics: 😉, 👍, and 👏.";

    const TXT_NEW: &str = "** Paragraph 2
Now, let's explore some emotional extremes 🌊. We've got your ecstatic face 🤩, your devastated face 😭, and your utterly confused face 🤯. But that's not all! 🤔 We've also got some subtle emotions like 😐, 🙃, and 👀.";

    // Delta generated from python library [diff-match-patch](https://pypi.org/project/diff-match-patch/) for the above text
    const DELTA: &str = "=15\t-1\t+Now, l\t=5\t-3\t+explo\t=1\t-6\t+e\t=6\t-1\t+emotion\t=1\t-3\t+l extreme\t=2\t-1\t+%F0%9F%8C%8A\t=17\t+ec\t=3\t-8\t+t\t=1\t-3\t+c\t=6\t-1\t+%F0%9F%A4%A9\t=7\t+deva\t=1\t+t\t=1\t+te\t=7\t-2\t+%F0%9F%98%AD\t=11\t-3\t+utte\t=1\t+l\t=2\t+confused \t=5\t-1\t+%F0%9F%A4%AF\t=6\t-6\t=2\t-3\t+at\t=3\t-1\t+n\t=1\t-2\t+t all\t=2\t-1\t+%F0%9F%A4%94\t=21\t-9\t+subt\t=2\t-1\t=15\t-1\t+%F0%9F%98%90\t=2\t-1\t+%F0%9F%99%83\t=6\t-55\t+%F0%9F%91%80\t=1";

    let dmp = DiffMatchPatch::default();
    let diffs = dmp.diff_from_delta::<Compat>(TXT_OLD, DELTA)?;
    let patches = dmp.patch_make(PatchInput::new_text_diffs(TXT_OLD, &diffs))?;
    let (txt_new, _) = dmp.patch_apply(&patches, TXT_OLD)?;

    assert_eq!(TXT_NEW, &txt_new);

    Ok(())
}

// JavaScript Compat
// Check https://github.com/AnubhabB/diff-match-patch-rs-bench/node/compat.js
// TODO: we'll have to do this in browser because `encodeURI` wont work
// #[test]
// fn test_compat_js_patch_text() -> Result<(), Error> {
//     const TXT_OLD: &str = "** Paragraph 2
// Let's start with some basics 😊. We've got your standard smiley face 🙂, your sad face ☹️, and your angry face 😠. But wait, there's more! 🤩 We've also got some more complex emotions like 😍, 🤤, and 🚀. And let's not forget about the classics: 😉, 👍, and 👏.";

//     const TXT_NEW: &str = "** Paragraph 2
// Now, let's explore some emotional extremes 🌊. We've got your ecstatic face 🤩, your devastated face 😭, and your utterly confused face 🤯. But that's not all! 🤔 We've also got some subtle emotions like 😐, 🙃, and 👀.";

//     // Patch generated from python library [diff-match-patch](https://www.npmjs.com/package/diff-match-patch) for the above text
//     const PATCH_TXT: &str = "@@ -12,38 +12,52 @@\n h 2%0A\n-Let's start with some basics %F0%9F%98%8A\n+Now, let's explore some emotional extremes %F0%9F%8C%8A\n . We\n@@ -73,48 +73,47 @@\n our \n+ec\n sta\n-ndard smiley\n+tic\n  face \n-%F0%9F%99%82\n+%F0%9F%A4%A9\n , your \n-sa\n+devastate\n d face \n-%E2%98%B9%EF%B8%8F\n+%F0%9F%98%AD\n , an\n@@ -123,47 +123,54 @@\n our \n-angry\n+utterly confused\n  face \n-%F0%9F%98%A0\n+%F0%9F%A4%AF\n . But \n-wait, there's more! %F0%9F%A4%A9\n+that's not all! %F0%9F%A4%94\n  We'\n@@ -190,20 +190,14 @@\n ome \n-more comp\n+subt\n le\n-x\n  emo\n@@ -211,70 +211,16 @@\n ike \n-%F0%9F%98%8D, %F0%9F%A4%A4\n+%F0%9F%98%90, %F0%9F%99%83\n , and \n-%F0%9F%9A%80. And let's not forget about the classics: %F0%9F%98%89, %F0%9F%91%8D, and %F0%9F%91%8F\n+%F0%9F%91%80\n .\n";

//     let dmp = DiffMatchPatch::default();
//     let patches = dmp.patch_from_text::<Compat>(PATCH_TXT)?;
//     let (txt_new, _) = dmp.patch_apply(&patches, TXT_OLD)?;

//     // Check if Python patch can be converted to new text
//     assert_eq!(TXT_NEW, &txt_new);

//     Ok(())
// }

// #[test]
// fn test_compat_js_patch_delta() -> Result<(), Error> {
//     const TXT_OLD: &str = "** Paragraph 2
// Let's start with some basics 😊. We've got your standard smiley face 🙂, your sad face ☹️, and your angry face 😠. But wait, there's more! 🤩 We've also got some more complex emotions like 😍, 🤤, and 🚀. And let's not forget about the classics: 😉, 👍, and 👏.";

//     const TXT_NEW: &str = "** Paragraph 2
// Now, let's explore some emotional extremes 🌊. We've got your ecstatic face 🤩, your devastated face 😭, and your utterly confused face 🤯. But that's not all! 🤔 We've also got some subtle emotions like 😐, 🙃, and 👀.";

//     // Delta generated from python library [diff-match-patch](https://www.npmjs.com/package/diff-match-patch) for the above text
//     const DELTA: &str = "=15\t-1\t+Now, l\t=5\t-3\t+explo\t=1\t-6\t+e\t=6\t-1\t+emotion\t=1\t-3\t+l extreme\t=2\t-1\t+%F0%9F%8C%8A\t=17\t+ec\t=3\t-8\t+t\t=1\t-3\t+c\t=6\t-1\t+%F0%9F%A4%A9\t=7\t+deva\t=1\t+t\t=1\t+te\t=7\t-2\t+%F0%9F%98%AD\t=11\t-3\t+utte\t=1\t+l\t=2\t+confused \t=5\t-1\t+%F0%9F%A4%AF\t=6\t-6\t=2\t-3\t+at\t=3\t-1\t+n\t=1\t-2\t+t all\t=2\t-1\t+%F0%9F%A4%94\t=21\t-9\t+subt\t=2\t-1\t=15\t-1\t+%F0%9F%98%90\t=2\t-1\t+%F0%9F%99%83\t=6\t-55\t+%F0%9F%91%80\t=1";

//     let dmp = DiffMatchPatch::default();
//     let diffs = dmp.diff_from_delta::<Compat>(TXT_OLD, DELTA)?;
//     let patches = dmp.patch_make(PatchInput::new_text_diffs(TXT_OLD, &diffs))?;
//     let (txt_new, _) = dmp.patch_apply(&patches, TXT_OLD)?;

//     assert_eq!(TXT_NEW, &txt_new);

//     Ok(())
// }
