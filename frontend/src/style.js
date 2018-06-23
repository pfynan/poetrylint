goTraverse = function (element) {
    var textSegs = [];
    var cur_ix = 0;
    Array.from(element.childNodes).forEach((node) => {
        switch(node.nodeType) {
            case Node.TEXT_NODE:
                textSegs.push({
                    'text': node.nodeValue,
                    'ix': cur_ix,
                    'node': node});
                cur_ix += node.nodeValue.length;
                break;
            case Node.ELEMENT_NODE:
                if(node.tagName === "DIV") {
                    var newSegs = goTraverse(node);
                    // Is a merge of newline for last line
                    // Breaks previous text -> insert newline if previous text
                    // exits
                    // Next starts on a new line... -> insert newline at the
                    // end, but only if it isn't there already
                    var cur_txt = textSegs.map(({text, ix, node}) => text).join('');
                    if(cur_txt.length > 0 && cur_txt.substring(cur_txt.length - 1) !== "\n") {
                        textSegs.push({
                            'text': "\n",
                            'ix': cur_ix,
                            'node': null});
                        cur_ix += 1;
                    }
                    textSegs.push({
                        'text': "",
                        'ix': cur_ix,
                        'node': node});
                    textSegs.push(...newSegs.map(({text, ix, node}) => (
                        {"text": text,
                        "ix": ix + cur_ix,
                        "node": node})));
                    var new_txt = newSegs.map(({text, ix, node}) => text).join('');
                    cur_ix += new_txt.length;
                    cur_txt = textSegs.map(({text, ix, node}) => text).join('');
                    if(cur_txt !== "" && cur_txt.substring(cur_txt.length - 1) !== "\n") {
                        textSegs.push({
                            'text': "\n",
                            'ix': cur_ix,
                            'node': null});
                        cur_ix += 1;
                    }
                } else if(node.tagName === "BR") {
                    textSegs.push({
                        'text': "\n",
                        'ix': cur_ix,
                        'node': null});
                    cur_ix += 1;
                    textSegs.push({
                        'text': "",
                        'ix': cur_ix,
                        'node': node});
                } else {
                    var newSegs = goTraverse(node);
                    textSegs.push({
                        'text': "",
                        'ix': cur_ix,
                        'node': node});
                    textSegs.push(...newSegs.map(({text, ix, node}) => (
                        {"text": text,
                        "ix": ix + cur_ix,
                        "node": node})));
                    var txt = newSegs.map(({text, ix, node}) => text).join('');
                    cur_ix += txt.length;
                }
                break;
            default:
                throw new Error();
        }
    });
    return textSegs;
}
getTextFromInput = function (element) {
    var new_text = goTraverse(element).map(({text}) => text).join('');

    new_text = new_text.substring(0, new_text.length - 1); // Trim newline
    return new_text;
}

getSelectionPos = function (element) {
    var sel = window.getSelection();
    var anchorIndex = null;
    var focusIndex = null;
    var segs = goTraverse(element);
    console.log(segs);
    segs.forEach(({ix, node}) => {
        if(node === sel.anchorNode) {
            anchorIndex = ix + sel.anchorOffset;
        }
        if(node === sel.focusNode) {
            focusIndex = ix + sel.focusOffset;
        }
    });
    if(sel.anchorNode.nodeType !== Node.TEXT_NODE) {
        console.log("Selection for anchor is not text node");
    }
    if(sel.focusNode.nodeType !== Node.TEXT_NODE) {
        console.log("Selection for focus is not text node");
    }
    console.log(sel);
    console.log(`Offsets: ${anchorIndex}, ${focusIndex}`);
    return {"anchorIndex": anchorIndex, "focusIndex": focusIndex};
}


setSelectionPos = function (element, {anchorIndex, focusIndex}) {
    var sel = window.getSelection();
    var anchorNode = null;
    var focusNode = null;
    var anchorOffset = null;
    var focusOffset = null;

    var segs = goTraverse(element);
    console.log(segs);
    segs.forEach(({ix, text, node}) => {
        if(anchorIndex >= ix && anchorIndex <= ix + text.length && node !== null) {
            anchorNode = node;
            anchorOffset = anchorIndex - ix;
        }
        if(focusIndex >= ix && focusIndex <= ix + text.length && node !== null) {
            focusNode = node;
            focusOffset = focusIndex - ix;
        }
    });

    console.log([anchorNode, anchorOffset, focusNode, focusOffset]);
    sel.setBaseAndExtent(anchorNode, anchorOffset, focusNode, focusOffset);
}


styleContentNodes = function (element, text, styles) {

    var new_text = getTextFromInput(element);
    var styles_exp = [];
    var start_ix = 0;
    styles.forEach( ({start, length, style}) => {
        if(start > start_ix) {
            styles_exp.push({"start": start_ix, "length": start - start_ix, "style": "none"});
        }
        styles_exp.push({"start": start, "length": length, "style": style});
        start_ix = start + length;
    });
    if(start_ix < new_text.length) {
        styles_exp.push({"start": start_ix, "length": new_text.length - start_ix, "style": "none"});
    }

    var segs = styles_exp.map(({start, length, style}) => {
        var txt = new_text.slice(start, start + length);
        if(style === "none") {
            return txt;
        } else {
            return `<span class="${style}">${txt}</span>`;
        }
    });

    var my_sel = getSelectionPos(element);
    // element.innerHTML = segs.join('').replace(/\n/g, "<br>");
    element.innerHTML = segs.join('').split("\n").map(a => {
        if(a.length > 0) {
            return "<div>" + a + "</div>";
        } else {
            return "<div><br></div>";
        }
    }).join('');
    setSelectionPos(element, my_sel);

}
