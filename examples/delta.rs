use diff_match_patch_rs::{DiffMatchPatch, Efficient, Error, PatchInput};

/// An example flow of the effitient mode
/// This demo will cover creating a diff of two texts and then `patching` it back to get the original text

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

    // When dealing with large text blocks and if you want to transmit this diff, `delta` will give you a minimal over the air representation of diffs
    // We'll use this delta string to recreate `diffs` at the destination and then create patches to apply
    let delta = dmp.diff_to_delta(&diffs)?;

    // lets see how our delta looks
    println!("{delta:?}");
    // You should see something like this
    // =25\t-1\t+carto\t=1\t-3\t=2\t-8\t+i\t=1\t-2\t+dividu\t=4\t-4\t+My\t=1\t-1\t+a\t=1\t-3\t+i\t=6\t+'s\t=1\t-5\t+comic\t=1\t-1\t=1\t-1\t=2\t-1\t+u\t=1\t-2\t+usu\t=8\t+whi\t=1\t+s\t=1\t-3\t+c\t=5\t+'m\t=1\t-5\t+qui\t=1\t-1\t=2\t-13\t=1\t-1\t=1\t-1\t+ept\t=2\t+t fu\t=1\t-1\t+ny\t=1\t-1\t+gags,\t=1\t-2\t+c\t=1\t-1\t+m\t=1\t+dic\t=4\t+ory\t=1\t-6\t+I\t=2\t-4\t+ave \t=1\t-2\t+e\t=1\t-1\t+d\t=7\t-1\t+wicked puns \t=1\t-2\t+nd s\t=1\t-1\t+upid j\t=1\t-1\t+kes\t=4\t-1\t+anvils th\t=2\t-1\t+ d\t=1\t-1\t=1\t+p \t=1\t-3\t=2\t+y\t=1\t-3\t+u\t=2\t-3\t+h\t=1\t-5\t=1\t-1\t+d\t=3\t-1\t+Now, l\t=5\t-3\t+explo\t=1\t-6\t+e\t=6\t-3\t+emot\t=1\t-1\t+onal extreme\t=4\t-1\t+%8C\t=18\t+ec\t=3\t-8\t+t\t=1\t-3\t+c\t=8\t-2\t+%A4%A9\t=7\t+deva\t=1\t+t\t=1\t+te\t=7\t-1\t+%F0%9F\t=1\t-4\t+%AD\t=11\t-3\t+utte\t=1\t+l\t=2\t+confused \t=7\t-2\t+%A4%AF\t=6\t-1\t+th\t=1\t-1\t=1\t-7\t=3\t-1\t+n\t=1\t-2\t+t all\t=5\t-1\t+%94\t=21\t-9\t+subt\t=2\t-1\t=18\t-64\t+%90\t=4\t-2\t+%99%83\t=9\t-1\t+%80\t=1

    Ok(delta)
}

fn at_destination(delta: &str) -> Result<(), Error> {
    // initializing the module
    let dmp = DiffMatchPatch::new();

    // lets recreate the diffs from the minimal `delta` string
    let delta = dmp.diff_from_delta::<Efficient>(TXT_OLD, delta)?;
    // Additional step of conveting `delta` -> `patches`
    let patches = dmp.patch_make(PatchInput::new_diffs(&delta))?;

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
    // At the source of diff where the old text is being edited we'll create a `delta` - a `delta` is a minimal representation of `diffs`
    let delta = at_source()?;

    // We'll send this diff to some destination e.g. db or the client where these changes are going to be applied

    // The destination will receive the patch string and will apply the patches to recreate the edits
    at_destination(&delta)
}
