/**
 * Web Component that manages a clickable and toggleable button that contains a fontawesome icon.
 * 
 * Styling for the button, icon and label is managed inside the component.css file in this directory.
 * 
 * @author Sebastian Lukas Hauer
 */

/**
 * Web Component Template for deep copying.
 */
const template = document.createElement("template");
template.innerHTML = String.raw
`<style>
    @import "support/vendor/fontawesome/css/all.css";

    .icon-button {
        padding: var(--awesome-button-padding);

        font-size: var(--awesome-button-font-size);

        background: var(--awesome-button-background);
        color: var(--awesome-button-color);

        border: var(--awesome-button-border);
        border-radius: var(--awesome-button-border-radius);
    }

    .icon-button.toggled {
        color: var(--awesome-button-toggled-color);
        background: var(--awesome-button-toggled-background);
    }
    
    .icon-button:hover {
        color: var(--awesome-button-hover-color);
        background: var(--awesome-button-hover-background);
    }

    .icon-button:active {
        color: var(--awesome-button-active-color);
        background: var(--awesome-button-active-background);
    }

    .icon-button-icon {
        display: block;
    }
</style>
<button class="icon-button">
    <i class="icon-button-icon"></i>
    <span class="icon-label">Label</span>
</button>`

/**
 * Component class to be registered as a custom HTMLElement.
 */
class AwesomeButton extends HTMLElement {
    constructor() {
        super();

        this._shadowRoot = this.attachShadow({ mode: "open" });
        this._shadowRoot.appendChild(template.content.cloneNode(true));

        this.$iconButton = this._shadowRoot.querySelector(".icon-button");
        this.$iconElement = this._shadowRoot.querySelector(".icon-button-icon");
        this.$iconLabel = this._shadowRoot.querySelector(".icon-label");

        this._icon = undefined;
        this._iconStyle = undefined;
        this._label = undefined;

        this._toggled = "false";
    }

    static get observedAttributes() {
        return ["label", "icon", "icon-style", "toggled"];
    }

    get label() {
        return this._label;
    }

    set label(value) {
        this.setAttribute("label", value);
    }

    get icon() {
        return this._icon;
    }

    set icon(value) {
        this.setAttribute("icon", value);
    }

    get iconStyle() {
        return this._iconStyle;
    }

    set iconStyle(value) {
        this.setAttribute("icon-style", value);
    }

    get toggled() {
        return this._toggled;
    }

    set toggled(value) {
        this.setAttribute("toggled", value);
    }

    toggle() {
        if(this.toggled === "true") { //The passed and stored values are, sadly, strings ...
            this.toggled = "false";
        } else {
            this.toggled = "true";
        }
    }

    attributeChangedCallback(name, oldVal, newVal) {
        if(name === "icon") {
            this.$iconElement.classList.remove(oldVal);
            this._icon = newVal;
            this.$iconElement.classList.add(newVal);
        }
        if(name === "icon-style") {
            this.$iconElement.classList.remove(oldVal);
            this._iconStyle = newVal;
            this.$iconElement.classList.add(newVal);
        }
        if(name === "label") {
            this._label = newVal;
        }
        if(name == "toggled") {
            this._toggled = newVal;
            if(this._toggled === "true") {
                this.$iconButton.classList.add("toggled");
            } else {
                this.$iconButton.classList.remove("toggled");
            }
        }
        this.render();
    }
    
    /**
     * Styled after the web components tutorial, even though this function is rather redundant.
     */
    render() {
        this.$iconLabel.innerHTML = this._label;
    }
}

window.customElements.define('awesome-button', AwesomeButton);