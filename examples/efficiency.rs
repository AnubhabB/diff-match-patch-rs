use diff_match_patch_rs::{DiffMatchPatch, Efficient, Error, PatchInput};

/// An example flow of the effitient mode
/// This demo will cover creating a diff of two texts and then `patching` it back to get the original text
///
/// NOTE:
/// This is the `efficiency` mode, here we apply `Diff` and `Patch` operations in `raw bytes &[u8]`.
/// `Efficiency mode` is called by using `dmp.diff_main::<Efficient>()` or `dmp.patch_from_text::<Efficient>()` APIs
/// `Efficiency mode` is not compatible with other libraries or implementations of `diff-match-patch`.
/// Use `efficiency` mode ONLY if you are using this `crate` across your stack
/// If you want a standardized implementation (working across libraries), use `Compat` mode istead

// This is the source text
const TXT_OLD: &str = "I am the very model of a modern Major-General,
I've information vegetable, animal, and mineral,
I know the kings of England, and I quote the fights historical,
From Marathon to Waterloo, in order categorical.

Let's start with some basics 😊. We've got your standard smiley face 🙂, your sad face ☹️, and your angry face 😠. But wait, there's more! 🤩 We've also got some more complex emotions like 😍, 🤤, and 🚀. And let's not forget about the classics: 😉, 👍, and 👏.";

// Let's assume this to be the text that was editted from the source text
const TXT_NEW: &str = "I am the very model of a cartoon individual,
My animation's comical, unusual, and whimsical,
I'm quite adept at funny gags, comedic theory I have read,
From wicked puns and stupid jokes to anvils that drop on your head.

Now, let's explore some emotional extremes 🌊. We've got your ecstatic face 🤩, your devastated face 😭, and your utterly confused face 🤯. But that's not all! 🤔 We've also got some subtle emotions like 😐, 🙃, and 👀.";

// An example of a function that creates a diff and returns a set of patches serialized
fn at_source() -> Result<String, Error> {
    // initializing the module
    let dmp = DiffMatchPatch::new();

    // create a list of diffs
    let diffs = dmp.diff_main::<Efficient>(TXT_OLD, TXT_NEW)?;

    // Now, we are going to create a list of `patches` to be applied to the old text to get the new text
    let patches = dmp.patch_make(PatchInput::new_diffs(&diffs))?;

    // in the real world you are going to transmit or store this diff serialized to undiff format to be consumed or used somewhere elese
    let patch_txt = dmp.patch_to_text(&patches);

    // lets see how our patches look
    println!("{patch_txt:?}");
    // You should see something like this
    // @@ -22,225 +22,250 @@\n f a \n-m\n+carto\n o\n-der\n n \n-Major-Ge\n+i\n n\n-er\n+dividu\n al,%0A\n-I've\n+My\n  \n-i\n+a\n n\n-for\n+i\n mation\n+'s\n  \n-veget\n+comic\n a\n-b\n l\n-e\n , \n-a\n+u\n n\n-im\n+usu\n al, and \n+whi\n m\n+s\n i\n-ner\n+c\n al,%0AI\n+'m\n  \n-know \n+qui\n t\n-h\n e \n-kings of Engl\n a\n-n\n d\n-,\n+ept\n  a\n+t fu\n n\n-d\n+ny\n  \n-I\n+gags,\n  \n-qu\n+c\n o\n-t\n+m\n e\n+dic\n  the\n+ory\n  \n-fights\n+I\n  h\n-isto\n+ave \n r\n-ic\n+e\n a\n-l\n+d\n ,%0AFrom \n-M\n+wicked puns \n a\n-ra\n+nd s\n t\n-h\n+upid j\n o\n-n\n+kes\n  to \n-W\n+anvils th\n at\n-e\n+ d\n r\n-l\n o\n+p \n o\n-, i\n n \n+y\n o\n-rde\n+u\n r \n-cat\n+h\n e\n-goric\n a\n-l\n+d\n .%0A%0A\n-L\n+Now, l\n et's \n-sta\n+explo\n r\n-t with\n+e\n  some \n-bas\n+emot\n i\n-c\n+onal extreme\n s %F0%9F\n-%98\n+%8C\n %8A. W\n@@ -282,55 +282,53 @@\n our \n+ec\n sta\n-ndard sm\n+t\n i\n-ley\n+c\n  face %F0%9F\n-%99%82\n+%A4%A9\n , your \n+deva\n s\n+t\n a\n+te\n d face \n-%E2\n+%F0%9F\n %98\n-%B9%EF%B8%8F\n+%AD\n , an\n@@ -338,53 +338,60 @@\n our \n-ang\n+utte\n r\n+l\n y \n+confused \n face %F0%9F\n-%98%A0\n+%A4%AF\n . But \n-w\n+th\n a\n-i\n t\n-, there\n 's \n-m\n+n\n o\n-re\n+t all\n ! %F0%9F%A4\n-%A9\n+%94\n  We'\n@@ -411,20 +411,14 @@\n ome \n-more comp\n+subt\n le\n-x\n  emo\n@@ -435,78 +435,15 @@\n  %F0%9F%98\n-%8D, %F0%9F%A4%A4, and %F0%9F%9A%80. And let's not forget about the classics: %F0%9F%98%89\n+%90\n , %F0%9F\n-%91%8D\n+%99%83\n , an\n@@ -451,6 +451,6 @@\n  %F0%9F%91\n-%8F\n+%80\n .\n

    Ok(patch_txt)
}

fn at_destination(patches: &str) -> Result<(), Error> {
    // initializing the module
    let dmp = DiffMatchPatch::new();

    // lets recreate the diffs from patches
    let patches = dmp.patch_from_text::<Efficient>(patches)?;

    // Now, lets apply these patches to the `old_txt` which is the original to get the new text
    let (new_txt, ops) = dmp.patch_apply(&patches, TXT_OLD)?;

    // Lets print out if the ops succeeded or not
    ops.iter()
        .for_each(|&o| println!("{}", if o { "OK" } else { "FAIL" }));

    // If everything goes as per plan you should see
    // OK
    // OK
    // ... and so on

    // lets check out if our `NEW_TXT` (presumably the edited one)
    if new_txt != TXT_NEW {
        return Err(Error::InvalidInput);
    }

    println!("Wallah! Patch applied successfully!");

    Ok(())
}

fn main() -> Result<(), Error> {
    // At the source of diff where the old text is being edited we'll create a set of patches
    let patches = at_source()?;

    // We'll send this diff to some destination e.g. db or the client where these changes are going to be applied

    // The destination will receive the patch string and will apply the patches to recreate the edits
    at_destination(&patches)
}
