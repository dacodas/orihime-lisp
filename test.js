function update_definition()
{
    // let text_hash = "57de18132110d0e40c3d4e9358ab724c2ab809e2be1814dae71b01375d8da5e7";
    let text_hash = "651bf5a51a80f110d3ee1723f939ce98ad3e38abf9d28917fc8756df4aea4a74";

    let selection = window.getSelection()

    function reqListener () {
        console.log(this.responseText);
        result = document.evaluate("//div[@id=\"anki-text\"]", document);
        result.iterateNext().innerHTML = this.responseText;
        hide_words_and_set_onclick();
    }

    var oReq = new XMLHttpRequest();
    oReq.addEventListener("load", reqListener);
    oReq.open("POST", "/show-text?text-hash=" + text_hash);
    oReq.setRequestHeader("Accept", "vnd+orihime.text")
    oReq.send();
}

function add_child_word()
{

    let selection = window.getSelection()

    let body = JSON.stringify(["add-child-word-to-text",
                               selection.anchorNode.parentElement.id,
                               selection.toString(),
                               selection.toString()])

    function reqListener () {
        console.log(this.responseText);
        update_definition();
    }

    var oReq = new XMLHttpRequest();
    oReq.addEventListener("load", reqListener);
    oReq.open("POST", "/simple-lisp-rpc");
    oReq.send(body);

}
