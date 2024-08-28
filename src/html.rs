/// A struct representing some properties to control the [`pretty_html`] generation.
/// The `insert_tag`, `delete_tag` and `equality_tag` represents the `html` tag to wrap the part of the text being inserted, deleted or equality.
///
/// E.g. if `insert_tag` is set to `span`, the text to be inserted will be wrapped around with `<span>some text to insert</span>`.
/// `insert_tag` defaults to the `ins` tag, `delete_tag` defaults to the `del` tag and `equality_tag` defaults to `span`.
///
/// `nltobr` switch enables or disables replacing of `\n` in the text with a `<br>` element. Defaults to `true`
///
/// `insert_class`, `delete_class` or `equality_class` if set will be added as a `class="your css class"`. Defaults to `None`
///
/// `insert_style`, `delete_style` and `equality_style` would add css style property to the output.
/// E.g. if `insert_style: Some("background: yellow; color: purple")` is set the
/// `insert` part of the pretty html would look like `<ins style="background: yellow; color: purple">insert text</ins>`
pub struct HtmlConfig<'a> {
    insert_tag: &'a str,
    delete_tag: &'a str,
    equality_tag: &'a str,
    nltobr: bool,
    insert_class: Option<&'a str>,
    delete_class: Option<&'a str>,
    equality_class: Option<&'a str>,
    insert_style: Option<&'a str>,
    delete_style: Option<&'a str>,
    equality_style: Option<&'a str>,
}

impl<'a> Default for HtmlConfig<'a> {
    fn default() -> Self {
        Self {
            insert_tag: "ins",
            delete_tag: "del",
            equality_tag: "span",
            nltobr: true,
            insert_class: None,
            delete_class: None,
            equality_class: None,
            insert_style: None,
            delete_style: None,
            equality_style: None,
        }
    }
}

impl<'a> HtmlConfig<'a> {
    /// Creates a new instance of the struct with some defaults.
    ///
    /// `insert_tag` defaults to "ins".
    ///
    /// `delete_tag` defaults to "del"
    ///
    /// `equality_tag` defaults to "span"
    ///
    /// `nltobr` defaults to `true`
    ///
    /// Other fields defaults to `None`
    pub fn new() -> Self {
        Self::default()
    }

    pub(crate) fn insert_tag(&self) -> &str {
        self.insert_tag
    }

    /// Set the HTML tag to be used for text inserted
    pub fn set_insert_tag(&mut self, tag: &'a str) {
        self.insert_tag = tag;
    }

    pub(crate) fn delete_tag(&self) -> &str {
        self.delete_tag
    }

    /// Set the HTML tag to be used for text deleted
    pub fn set_delete_tag(&mut self, tag: &'a str) {
        self.delete_tag = tag;
    }

    pub(crate) fn equality_tag(&self) -> &str {
        self.equality_tag
    }

    /// Set the HTML tag to be used for text that has not changed
    pub fn set_equality_tag(&mut self, tag: &'a str) {
        self.equality_tag = tag;
    }

    pub(crate) fn nltobr(&self) -> bool {
        self.nltobr
    }

    /// Switch to control if `\nl` should be replaced with `<br>`
    pub fn set_nl_to_br(&mut self, nltobr: bool) {
        self.nltobr = nltobr
    }

    pub(crate) fn insert_class(&self) -> Option<&'a str> {
        self.insert_class
    }

    /// Set the css class for the text inserted
    pub fn set_insert_class(&mut self, class: Option<&'a str>) {
        self.insert_class = class;
    }

    pub(crate) fn delete_class(&self) -> Option<&'a str> {
        self.delete_class
    }

    /// Set the delete class for text deleted
    pub fn set_delete_class(&mut self, class: Option<&'a str>) {
        self.delete_class = class;
    }

    pub(crate) fn equality_class(&self) -> Option<&'a str> {
        self.equality_class
    }

    /// Set the css class for text that has not changed
    pub fn set_equality_class(&mut self, class: Option<&'a str>) {
        self.equality_class = class;
    }

    pub(crate) fn insert_style(&self) -> Option<&'a str> {
        self.insert_style
    }

    /// Set the css style property for text inserted
    pub fn set_insert_style(&mut self, style: Option<&'a str>) {
        self.insert_style = style;
    }

    pub(crate) fn delete_style(&self) -> Option<&'a str> {
        self.delete_style
    }

    /// Set the css style property for text deleted
    pub fn set_delete_style(&mut self, style: Option<&'a str>) {
        self.delete_style = style;
    }

    pub(crate) fn equality_style(&self) -> Option<&'a str> {
        self.equality_style
    }

    /// Set the css style for text that has not changed
    pub fn set_equality_style(&mut self, style: Option<&'a str>) {
        self.equality_style = style;
    }
}
