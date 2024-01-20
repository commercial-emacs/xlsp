;; -*- lexical-binding: t -*-

[((name . "Definition")
  (type (kind . "or")
	(items .
	       [((kind . "reference") (name . "Location"))
		((kind . "array")
		 (element (kind . "reference") (name . "Location")))]))
  (documentation .
		 "The definition of a symbol represented as one or many [locations](#Location).\nFor most programming languages there is only one location at which a symbol is\ndefined.\n\nServers should prefer returning `DefinitionLink` over `Definition` if supported\nby the client."))
 ((name . "DefinitionLink")
  (type (kind . "reference") (name . "LocationLink"))
  (documentation .
		 "Information about where a symbol is defined.\n\nProvides additional metadata over normal [location](#Location) definitions, including the range of\nthe defining symbol"))
 ((name . "LSPArray")
  (type (kind . "array")
	(element (kind . "reference") (name . "LSPAny")))
  (documentation . "LSP arrays.\n@since 3.17.0") (since . "3.17.0"))
 ((name . "LSPAny")
  (type (kind . "or")
	(items .
	       [((kind . "reference") (name . "LSPObject"))
		((kind . "reference") (name . "LSPArray"))
		((kind . "base") (name . "string"))
		((kind . "base") (name . "integer"))
		((kind . "base") (name . "uinteger"))
		((kind . "base") (name . "decimal"))
		((kind . "base") (name . "boolean"))
		((kind . "base") (name . "null"))]))
  (documentation .
		 "The LSP any type.\nPlease note that strictly speaking a property with the value `undefined`\ncan't be converted into JSON preserving the property name. However for\nconvenience it is allowed and assumed that all these properties are\noptional as well.\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "Declaration")
  (type (kind . "or")
	(items .
	       [((kind . "reference") (name . "Location"))
		((kind . "array")
		 (element (kind . "reference") (name . "Location")))]))
  (documentation .
		 "The declaration of a symbol representation as one or many [locations](#Location)."))
 ((name . "DeclarationLink")
  (type (kind . "reference") (name . "LocationLink"))
  (documentation .
		 "Information about where a symbol is declared.\n\nProvides additional metadata over normal [location](#Location) declarations, including the range of\nthe declaring symbol.\n\nServers should prefer returning `DeclarationLink` over `Declaration` if supported\nby the client."))
 ((name . "InlineValue")
  (type (kind . "or")
	(items .
	       [((kind . "reference") (name . "InlineValueText"))
		((kind . "reference")
		 (name . "InlineValueVariableLookup"))
		((kind . "reference")
		 (name . "InlineValueEvaluatableExpression"))]))
  (documentation .
		 "Inline value information can be provided by different means:\n- directly as a text value (class InlineValueText).\n- as a name to use for a variable lookup (class InlineValueVariableLookup)\n- as an evaluatable expression (class InlineValueEvaluatableExpression)\nThe InlineValue types combines all inline value types into one type.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DocumentDiagnosticReport")
  (type (kind . "or")
	(items .
	       [((kind . "reference")
		 (name . "RelatedFullDocumentDiagnosticReport"))
		((kind . "reference")
		 (name . "RelatedUnchangedDocumentDiagnosticReport"))]))
  (documentation .
		 "The result of a document diagnostic pull request. A report can\neither be a full report containing all diagnostics for the\nrequested document or an unchanged report indicating that nothing\nhas changed in terms of diagnostics in comparison to the last\npull request.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "PrepareRenameResult")
  (type (kind . "or")
	(items .
	       [((kind . "reference") (name . "Range"))
		((kind . "literal")
		 (value
		  (properties .
			      [((name . "range")
				(type (kind . "reference")
				      (name . "Range")))
			       ((name . "placeholder")
				(type (kind . "base")
				      (name . "string")))])))
		((kind . "literal")
		 (value
		  (properties .
			      [((name . "defaultBehavior")
				(type (kind . "base")
				      (name . "boolean")))])))])))
 ((name . "ProgressToken")
  (type (kind . "or")
	(items .
	       [((kind . "base") (name . "integer"))
		((kind . "base") (name . "string"))])))
 ((name . "DocumentSelector")
  (type (kind . "array")
	(element (kind . "reference") (name . "DocumentFilter")))
  (documentation .
		 "A document selector is the combination of one or many document filters.\n\n@sample `let sel:DocumentSelector = [{ language: 'typescript' }, { language: 'json', pattern: '**∕tsconfig.json' }]`;\n\nThe use of a string as a document filter is deprecated @since 3.16.0.")
  (since . "3.16.0."))
 ((name . "ChangeAnnotationIdentifier")
  (type (kind . "base") (name . "string"))
  (documentation .
		 "An identifier to refer to a change annotation stored with a workspace edit."))
 ((name . "WorkspaceDocumentDiagnosticReport")
  (type (kind . "or")
	(items .
	       [((kind . "reference")
		 (name . "WorkspaceFullDocumentDiagnosticReport"))
		((kind . "reference")
		 (name . "WorkspaceUnchangedDocumentDiagnosticReport"))]))
  (documentation .
		 "A workspace diagnostic document report.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "TextDocumentContentChangeEvent")
  (type (kind . "or")
	(items .
	       [((kind . "literal")
		 (value
		  (properties .
			      [((name . "range")
				(type (kind . "reference")
				      (name . "Range"))
				(documentation .
					       "The range of the document that changed."))
			       ((name . "rangeLength")
				(type (kind . "base")
				      (name . "uinteger"))
				(optional . t)
				(documentation .
					       "The optional length of the range that got replaced.\n\n@deprecated use range instead."))
			       ((name . "text")
				(type (kind . "base")
				      (name . "string"))
				(documentation .
					       "The new text for the provided range."))])))
		((kind . "literal")
		 (value
		  (properties .
			      [((name . "text")
				(type (kind . "base")
				      (name . "string"))
				(documentation .
					       "The new text of the whole document."))])))]))
  (documentation .
		 "An event describing a change to a text document. If only a text is provided\nit is considered to be the full content of the document."))
 ((name . "MarkedString")
  (type (kind . "or")
	(items .
	       [((kind . "base") (name . "string"))
		((kind . "literal")
		 (value
		  (properties .
			      [((name . "language")
				(type (kind . "base")
				      (name . "string")))
			       ((name . "value")
				(type (kind . "base")
				      (name . "string")))])))]))
  (documentation .
		 "MarkedString can be used to render human readable text. It is either a markdown string\nor a code-block that provides a language and a code snippet. The language identifier\nis semantically equal to the optional language identifier in fenced code blocks in GitHub\nissues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting\n\nThe pair of a language and a value is an equivalent to markdown:\n```${language}\n${value}\n```\n\nNote that markdown strings will be sanitized - that means html will be escaped.\n@deprecated use MarkupContent instead."))
 ((name . "DocumentFilter")
  (type (kind . "or")
	(items .
	       [((kind . "reference") (name . "TextDocumentFilter"))
		((kind . "reference")
		 (name . "NotebookCellTextDocumentFilter"))]))
  (documentation .
		 "A document filter describes a top level text document or\na notebook cell document.\n\n@since 3.17.0 - proposed support for NotebookCellTextDocumentFilter.")
  (since .
	 "3.17.0 - proposed support for NotebookCellTextDocumentFilter."))
 ((name . "GlobPattern")
  (type (kind . "or")
	(items .
	       [((kind . "reference") (name . "Pattern"))
		((kind . "reference") (name . "RelativePattern"))]))
  (documentation .
		 "The glob pattern. Either a string pattern or a relative pattern.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "TextDocumentFilter")
  (type (kind . "or")
	(items .
	       [((kind . "literal")
		 (value
		  (properties .
			      [((name . "language")
				(type (kind . "base")
				      (name . "string"))
				(documentation .
					       "A language id, like `typescript`."))
			       ((name . "scheme")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation .
					       "A Uri [scheme](#Uri.scheme), like `file` or `untitled`."))
			       ((name . "pattern")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation .
					       "A glob pattern, like `*.{ts,js}`."))])))
		((kind . "literal")
		 (value
		  (properties .
			      [((name . "language")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation .
					       "A language id, like `typescript`."))
			       ((name . "scheme")
				(type (kind . "base")
				      (name . "string"))
				(documentation .
					       "A Uri [scheme](#Uri.scheme), like `file` or `untitled`."))
			       ((name . "pattern")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation .
					       "A glob pattern, like `*.{ts,js}`."))])))
		((kind . "literal")
		 (value
		  (properties .
			      [((name . "language")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation .
					       "A language id, like `typescript`."))
			       ((name . "scheme")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation .
					       "A Uri [scheme](#Uri.scheme), like `file` or `untitled`."))
			       ((name . "pattern")
				(type (kind . "base")
				      (name . "string"))
				(documentation .
					       "A glob pattern, like `*.{ts,js}`."))])))]))
  (documentation .
		 "A document filter denotes a document by different properties like\nthe [language](#TextDocument.languageId), the [scheme](#Uri.scheme) of\nits resource, or a glob-pattern that is applied to the [path](#TextDocument.fileName).\n\nGlob patterns can have the following syntax:\n- `*` to match one or more characters in a path segment\n- `?` to match on one character in a path segment\n- `**` to match any number of path segments, including none\n- `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)\n- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)\n- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)\n\n@sample A language filter that applies to typescript files on disk: `{ language: 'typescript', scheme: 'file' }`\n@sample A language filter that applies to all package.json paths: `{ language: 'json', pattern: '**package.json' }`\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "NotebookDocumentFilter")
  (type (kind . "or")
	(items .
	       [((kind . "literal")
		 (value
		  (properties .
			      [((name . "notebookType")
				(type (kind . "base")
				      (name . "string"))
				(documentation .
					       "The type of the enclosing notebook."))
			       ((name . "scheme")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation .
					       "A Uri [scheme](#Uri.scheme), like `file` or `untitled`."))
			       ((name . "pattern")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation . "A glob pattern."))])))
		((kind . "literal")
		 (value
		  (properties .
			      [((name . "notebookType")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation .
					       "The type of the enclosing notebook."))
			       ((name . "scheme")
				(type (kind . "base")
				      (name . "string"))
				(documentation .
					       "A Uri [scheme](#Uri.scheme), like `file` or `untitled`."))
			       ((name . "pattern")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation . "A glob pattern."))])))
		((kind . "literal")
		 (value
		  (properties .
			      [((name . "notebookType")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation .
					       "The type of the enclosing notebook."))
			       ((name . "scheme")
				(type (kind . "base")
				      (name . "string"))
				(optional . t)
				(documentation .
					       "A Uri [scheme](#Uri.scheme), like `file` or `untitled`."))
			       ((name . "pattern")
				(type (kind . "base")
				      (name . "string"))
				(documentation . "A glob pattern."))])))]))
  (documentation .
		 "A notebook document filter denotes a notebook document by\ndifferent properties. The properties will be match\nagainst the notebook's URI (same as with documents)\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "Pattern") (type (kind . "base") (name . "string"))
  (documentation .
		 "The glob pattern to watch relative to the base path. Glob patterns can have the following syntax:\n- `*` to match one or more characters in a path segment\n- `?` to match on one character in a path segment\n- `**` to match any number of path segments, including none\n- `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)\n- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)\n- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)\n\n@since 3.17.0")
  (since . "3.17.0"))]
