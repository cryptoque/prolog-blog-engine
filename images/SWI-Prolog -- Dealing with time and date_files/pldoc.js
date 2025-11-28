/*  This file is  part  of  the   SWI-Prolog  PlDoc  package  for online
    documentation browsing. It defines JavaScript to issue HTTP requests
    on the Prolog server that do not create a new page and handling
    footnotes.

Author:  Jan Wielemaker & Michiel Hildebrand
Copying: Public domain
*/

function HTTPrequest(url)
{ $.get(url,
	{
	});
}

function selectAndCopy(el, clean) {
  let text = el.innerText;
  if ( clean )
    text = text.replace(/[-+?@:]/g, "");
  const selection = window.getSelection();
  const range = document.createRange();
  range.selectNodeContents(el);
  selection.removeAllRanges();
  selection.addRange(range);
  navigator.clipboard.writeText(text);
}

function selectAndCopyPred(ev) {
  const el = ev.target;
  if ( el.tagName == "VAR" )
    selectAndCopy(el.closest("a"), true)
  else
    selectAndCopy(el, false)
}

function copyCode() {
  $("code.copy").click(function(ev) {
    selectAndCopy(ev.target)
  });
}

function copyMode() {
  $("dt.pubdef a, multidef a").each(function() {
    const el = $(this);
    if ( !el.hasClass("source") ) {
      el.addClass("copy")
	.click(selectAndCopyPred);
    }
  });
}

function setupFootnotes() {
  const footnoteactivator = $('.fn');
  footnoteactivator.mouseenter(function() {
    window.clearTimeout(this.footnoteid);
    var fn = $(this).find('span.fn-text');
    if ( fn ) {
      fn.removeClass('fn-text');
      fn.addClass('fnp');
    }
    $(this).find('span.fnp').show(100);
  });
  footnoteactivator.mouseleave(function() {
    const t = $(this).find('span.fnp');
    if(this.footnoteid !== null)
    { window.clearTimeout(this.footnoteid);
    }

    this.footnoteid = window.setTimeout(
      function() {
	t.hide(100);
      }, 2000);
  });
}

/* Improve footnote interaction.  Contributed by Anne Ogborn.
*/

$(function(){
  copyCode();
  copyMode();
  setupFootnotes();
});
