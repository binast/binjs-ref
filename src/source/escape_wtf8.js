module.exports = function (s) {
	/* See crates/binjs_io/src/escaped_wtf8.rs */
	return s.replace(/[\u007F\uD800-\uDFFF]/ug, function(m) {{
		if (m == "\u007F") {{
			return "\u007F007F";
		}}
		return "\u007F" + m.charCodeAt(0).toString(16);
	}});
}
