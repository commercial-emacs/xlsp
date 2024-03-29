;; -*- lexical-binding: t -*-

[((name . "SemanticTokenTypes")
  (type (kind . "base") (name . "string"))
  (values .
	  [((name . "namespace") (value . "namespace"))
	   ((name . "type") (value . "type")
	    (documentation .
			   "Represents a generic type. Acts as a fallback for types which can't be mapped to\na specific type like class or enum."))
	   ((name . "class") (value . "class"))
	   ((name . "enum") (value . "enum"))
	   ((name . "interface") (value . "interface"))
	   ((name . "struct") (value . "struct"))
	   ((name . "typeParameter") (value . "typeParameter"))
	   ((name . "parameter") (value . "parameter"))
	   ((name . "variable") (value . "variable"))
	   ((name . "property") (value . "property"))
	   ((name . "enumMember") (value . "enumMember"))
	   ((name . "event") (value . "event"))
	   ((name . "function") (value . "function"))
	   ((name . "method") (value . "method"))
	   ((name . "macro") (value . "macro"))
	   ((name . "keyword") (value . "keyword"))
	   ((name . "modifier") (value . "modifier"))
	   ((name . "comment") (value . "comment"))
	   ((name . "string") (value . "string"))
	   ((name . "number") (value . "number"))
	   ((name . "regexp") (value . "regexp"))
	   ((name . "operator") (value . "operator"))
	   ((name . "decorator") (value . "decorator")
	    (documentation . "@since 3.17.0") (since . "3.17.0"))])
  (supportsCustomValues . t)
  (documentation .
		 "A set of predefined token types. This set is not fixed\nan clients can specify additional token types via the\ncorresponding client capabilities.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "SemanticTokenModifiers")
  (type (kind . "base") (name . "string"))
  (values .
	  [((name . "declaration") (value . "declaration"))
	   ((name . "definition") (value . "definition"))
	   ((name . "readonly") (value . "readonly"))
	   ((name . "static") (value . "static"))
	   ((name . "deprecated") (value . "deprecated"))
	   ((name . "abstract") (value . "abstract"))
	   ((name . "async") (value . "async"))
	   ((name . "modification") (value . "modification"))
	   ((name . "documentation") (value . "documentation"))
	   ((name . "defaultLibrary") (value . "defaultLibrary"))])
  (supportsCustomValues . t)
  (documentation .
		 "A set of predefined token modifiers. This set is not fixed\nan clients can specify additional token types via the\ncorresponding client capabilities.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "DocumentDiagnosticReportKind")
  (type (kind . "base") (name . "string"))
  (values .
	  [((name . "Full") (value . "full")
	    (documentation .
			   "A diagnostic report with a full\nset of problems."))
	   ((name . "Unchanged") (value . "unchanged")
	    (documentation .
			   "A report indicating that the last\nreturned report is still accurate."))])
  (documentation .
		 "The document diagnostic report kinds.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "ErrorCodes") (type (kind . "base") (name . "integer"))
  (values .
	  [((name . "ParseError") (value . -32700))
	   ((name . "InvalidRequest") (value . -32600))
	   ((name . "MethodNotFound") (value . -32601))
	   ((name . "InvalidParams") (value . -32602))
	   ((name . "InternalError") (value . -32603))
	   ((name . "ServerNotInitialized") (value . -32002)
	    (documentation .
			   "Error code indicating that a server received a notification or\nrequest before the server has received the `initialize` request."))
	   ((name . "UnknownErrorCode") (value . -32001))])
  (supportsCustomValues . t)
  (documentation . "Predefined error codes."))
 ((name . "LSPErrorCodes") (type (kind . "base") (name . "integer"))
  (values .
	  [((name . "RequestFailed") (value . -32803)
	    (documentation .
			   "A request failed but it was syntactically correct, e.g the\nmethod name was known and the parameters were valid. The error\nmessage should contain human readable information about why\nthe request failed.\n\n@since 3.17.0")
	    (since . "3.17.0"))
	   ((name . "ServerCancelled") (value . -32802)
	    (documentation .
			   "The server cancelled the request. This error code should\nonly be used for requests that explicitly support being\nserver cancellable.\n\n@since 3.17.0")
	    (since . "3.17.0"))
	   ((name . "ContentModified") (value . -32801)
	    (documentation .
			   "The server detected that the content of a document got\nmodified outside normal conditions. A server should\nNOT send this error code if it detects a content change\nin it unprocessed messages. The result even computed\non an older state might still be useful for the client.\n\nIf a client decides that a result is not of any use anymore\nthe client should cancel the request."))
	   ((name . "RequestCancelled") (value . -32800)
	    (documentation .
			   "The client has canceled a request and a server as detected\nthe cancel."))])
  (supportsCustomValues . t))
 ((name . "FoldingRangeKind") (type (kind . "base") (name . "string"))
  (values .
	  [((name . "Comment") (value . "comment")
	    (documentation . "Folding range for a comment"))
	   ((name . "Imports") (value . "imports")
	    (documentation . "Folding range for an import or include"))
	   ((name . "Region") (value . "region")
	    (documentation .
			   "Folding range for a region (e.g. `#region`)"))])
  (supportsCustomValues . t)
  (documentation . "A set of predefined range kinds."))
 ((name . "SymbolKind") (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "File") (value . 1))
	   ((name . "Module") (value . 2))
	   ((name . "Namespace") (value . 3))
	   ((name . "Package") (value . 4))
	   ((name . "Class") (value . 5))
	   ((name . "Method") (value . 6))
	   ((name . "Property") (value . 7))
	   ((name . "Field") (value . 8))
	   ((name . "Constructor") (value . 9))
	   ((name . "Enum") (value . 10))
	   ((name . "Interface") (value . 11))
	   ((name . "Function") (value . 12))
	   ((name . "Variable") (value . 13))
	   ((name . "Constant") (value . 14))
	   ((name . "String") (value . 15))
	   ((name . "Number") (value . 16))
	   ((name . "Boolean") (value . 17))
	   ((name . "Array") (value . 18))
	   ((name . "Object") (value . 19))
	   ((name . "Key") (value . 20))
	   ((name . "Null") (value . 21))
	   ((name . "EnumMember") (value . 22))
	   ((name . "Struct") (value . 23))
	   ((name . "Event") (value . 24))
	   ((name . "Operator") (value . 25))
	   ((name . "TypeParameter") (value . 26))])
  (documentation . "A symbol kind."))
 ((name . "SymbolTag") (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Deprecated") (value . 1)
	    (documentation .
			   "Render a symbol as obsolete, usually using a strike-out."))])
  (documentation .
		 "Symbol tags are extra annotations that tweak the rendering of a symbol.\n\n@since 3.16")
  (since . "3.16"))
 ((name . "UniquenessLevel") (type (kind . "base") (name . "string"))
  (values .
	  [((name . "document") (value . "document")
	    (documentation .
			   "The moniker is only unique inside a document"))
	   ((name . "project") (value . "project")
	    (documentation .
			   "The moniker is unique inside a project for which a dump got created"))
	   ((name . "group") (value . "group")
	    (documentation .
			   "The moniker is unique inside the group to which a project belongs"))
	   ((name . "scheme") (value . "scheme")
	    (documentation .
			   "The moniker is unique inside the moniker scheme."))
	   ((name . "global") (value . "global")
	    (documentation . "The moniker is globally unique"))])
  (documentation .
		 "Moniker uniqueness level to define scope of the moniker.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "MonikerKind") (type (kind . "base") (name . "string"))
  (values .
	  [((name . "import") (value . "import")
	    (documentation .
			   "The moniker represent a symbol that is imported into a project"))
	   ((name . "export") (value . "export")
	    (documentation .
			   "The moniker represents a symbol that is exported from a project"))
	   ((name . "local") (value . "local")
	    (documentation .
			   "The moniker represents a symbol that is local to a project (e.g. a local\nvariable of a function, a class not visible outside the project, ...)"))])
  (documentation . "The moniker kind.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "InlayHintKind") (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Type") (value . 1)
	    (documentation .
			   "An inlay hint that for a type annotation."))
	   ((name . "Parameter") (value . 2)
	    (documentation . "An inlay hint that is for a parameter."))])
  (documentation . "Inlay hint kinds.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "MessageType") (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Error") (value . 1)
	    (documentation . "An error message."))
	   ((name . "Warning") (value . 2)
	    (documentation . "A warning message."))
	   ((name . "Info") (value . 3)
	    (documentation . "An information message."))
	   ((name . "Log") (value . 4)
	    (documentation . "A log message."))])
  (documentation . "The message type"))
 ((name . "TextDocumentSyncKind")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "None") (value . 0)
	    (documentation . "Documents should not be synced at all."))
	   ((name . "Full") (value . 1)
	    (documentation .
			   "Documents are synced by always sending the full content\nof the document."))
	   ((name . "Incremental") (value . 2)
	    (documentation .
			   "Documents are synced by sending the full content on open.\nAfter that only incremental updates to the document are\nsend."))])
  (documentation .
		 "Defines how the host (editor) should sync\ndocument changes to the language server."))
 ((name . "TextDocumentSaveReason")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Manual") (value . 1)
	    (documentation .
			   "Manually triggered, e.g. by the user pressing save, by starting debugging,\nor by an API call."))
	   ((name . "AfterDelay") (value . 2)
	    (documentation . "Automatic after a delay."))
	   ((name . "FocusOut") (value . 3)
	    (documentation . "When the editor lost focus."))])
  (documentation . "Represents reasons why a text document is saved."))
 ((name . "CompletionItemKind")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Text") (value . 1))
	   ((name . "Method") (value . 2))
	   ((name . "Function") (value . 3))
	   ((name . "Constructor") (value . 4))
	   ((name . "Field") (value . 5))
	   ((name . "Variable") (value . 6))
	   ((name . "Class") (value . 7))
	   ((name . "Interface") (value . 8))
	   ((name . "Module") (value . 9))
	   ((name . "Property") (value . 10))
	   ((name . "Unit") (value . 11))
	   ((name . "Value") (value . 12))
	   ((name . "Enum") (value . 13))
	   ((name . "Keyword") (value . 14))
	   ((name . "Snippet") (value . 15))
	   ((name . "Color") (value . 16))
	   ((name . "File") (value . 17))
	   ((name . "Reference") (value . 18))
	   ((name . "Folder") (value . 19))
	   ((name . "EnumMember") (value . 20))
	   ((name . "Constant") (value . 21))
	   ((name . "Struct") (value . 22))
	   ((name . "Event") (value . 23))
	   ((name . "Operator") (value . 24))
	   ((name . "TypeParameter") (value . 25))])
  (documentation . "The kind of a completion entry."))
 ((name . "CompletionItemTag")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Deprecated") (value . 1)
	    (documentation .
			   "Render a completion as obsolete, usually using a strike-out."))])
  (documentation .
		 "Completion item tags are extra annotations that tweak the rendering of a completion\nitem.\n\n@since 3.15.0")
  (since . "3.15.0"))
 ((name . "InsertTextFormat")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "PlainText") (value . 1)
	    (documentation .
			   "The primary text to be inserted is treated as a plain string."))
	   ((name . "Snippet") (value . 2)
	    (documentation .
			   "The primary text to be inserted is treated as a snippet.\n\nA snippet can define tab stops and placeholders with `$1`, `$2`\nand `${3:foo}`. `$0` defines the final tab stop, it defaults to\nthe end of the snippet. Placeholders with equal identifiers are linked,\nthat is typing in one will update others too.\n\nSee also: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#snippet_syntax"))])
  (documentation .
		 "Defines whether the insert text in a completion item should be interpreted as\nplain text or a snippet."))
 ((name . "InsertTextMode") (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "asIs") (value . 1)
	    (documentation .
			   "The insertion or replace strings is taken as it is. If the\nvalue is multi line the lines below the cursor will be\ninserted using the indentation defined in the string value.\nThe client will not apply any kind of adjustments to the\nstring."))
	   ((name . "adjustIndentation") (value . 2)
	    (documentation .
			   "The editor adjusts leading whitespace of new lines so that\nthey match the indentation up to the cursor of the line for\nwhich the item is accepted.\n\nConsider a line like this: <2tabs><cursor><3tabs>foo. Accepting a\nmulti line completion item is indented using 2 tabs and all\nfollowing lines inserted will be indented using 2 tabs as well."))])
  (documentation .
		 "How whitespace and indentation is handled during completion\nitem insertion.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "DocumentHighlightKind")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Text") (value . 1)
	    (documentation . "A textual occurrence."))
	   ((name . "Read") (value . 2)
	    (documentation .
			   "Read-access of a symbol, like reading a variable."))
	   ((name . "Write") (value . 3)
	    (documentation .
			   "Write-access of a symbol, like writing to a variable."))])
  (documentation . "A document highlight kind."))
 ((name . "CodeActionKind") (type (kind . "base") (name . "string"))
  (values .
	  [((name . "Empty") (value . "")
	    (documentation . "Empty kind."))
	   ((name . "QuickFix") (value . "quickfix")
	    (documentation .
			   "Base kind for quickfix actions: 'quickfix'"))
	   ((name . "Refactor") (value . "refactor")
	    (documentation .
			   "Base kind for refactoring actions: 'refactor'"))
	   ((name . "RefactorExtract") (value . "refactor.extract")
	    (documentation .
			   "Base kind for refactoring extraction actions: 'refactor.extract'\n\nExample extract actions:\n\n- Extract method\n- Extract function\n- Extract variable\n- Extract interface from class\n- ..."))
	   ((name . "RefactorInline") (value . "refactor.inline")
	    (documentation .
			   "Base kind for refactoring inline actions: 'refactor.inline'\n\nExample inline actions:\n\n- Inline function\n- Inline variable\n- Inline constant\n- ..."))
	   ((name . "RefactorRewrite") (value . "refactor.rewrite")
	    (documentation .
			   "Base kind for refactoring rewrite actions: 'refactor.rewrite'\n\nExample rewrite actions:\n\n- Convert JavaScript function to class\n- Add or remove parameter\n- Encapsulate field\n- Make method static\n- Move method to base class\n- ..."))
	   ((name . "Source") (value . "source")
	    (documentation .
			   "Base kind for source actions: `source`\n\nSource code actions apply to the entire file."))
	   ((name . "SourceOrganizeImports")
	    (value . "source.organizeImports")
	    (documentation .
			   "Base kind for an organize imports source action: `source.organizeImports`"))
	   ((name . "SourceFixAll") (value . "source.fixAll")
	    (documentation .
			   "Base kind for auto-fix source actions: `source.fixAll`.\n\nFix all actions automatically fix errors that have a clear fix that do not require user input.\nThey should not suppress errors or perform unsafe fixes such as generating new types or classes.\n\n@since 3.15.0")
	    (since . "3.15.0"))])
  (supportsCustomValues . t)
  (documentation . "A set of predefined code action kinds"))
 ((name . "TraceValues") (type (kind . "base") (name . "string"))
  (values .
	  [((name . "Off") (value . "off")
	    (documentation . "Turn tracing off."))
	   ((name . "Messages") (value . "messages")
	    (documentation . "Trace messages only."))
	   ((name . "Verbose") (value . "verbose")
	    (documentation . "Verbose message tracing."))]))
 ((name . "MarkupKind") (type (kind . "base") (name . "string"))
  (values .
	  [((name . "PlainText") (value . "plaintext")
	    (documentation .
			   "Plain text is supported as a content format"))
	   ((name . "Markdown") (value . "markdown")
	    (documentation .
			   "Markdown is supported as a content format"))])
  (documentation .
		 "Describes the content type that a client supports in various\nresult literals like `Hover`, `ParameterInfo` or `CompletionItem`.\n\nPlease note that `MarkupKinds` must not start with a `$`. This kinds\nare reserved for internal usage."))
 ((name . "PositionEncodingKind")
  (type (kind . "base") (name . "string"))
  (values .
	  [((name . "UTF8") (value . "utf-8")
	    (documentation .
			   "Character offsets count UTF-8 code units."))
	   ((name . "UTF16") (value . "utf-16")
	    (documentation .
			   "Character offsets count UTF-16 code units.\n\nThis is the default and must always be supported\nby servers"))
	   ((name . "UTF32") (value . "utf-32")
	    (documentation .
			   "Character offsets count UTF-32 code units.\n\nImplementation note: these are the same as Unicode code points,\nso this `PositionEncodingKind` may also be used for an\nencoding-agnostic representation of character offsets."))])
  (supportsCustomValues . t)
  (documentation .
		 "A set of predefined position encoding kinds.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "FileChangeType") (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Created") (value . 1)
	    (documentation . "The file got created."))
	   ((name . "Changed") (value . 2)
	    (documentation . "The file got changed."))
	   ((name . "Deleted") (value . 3)
	    (documentation . "The file got deleted."))])
  (documentation . "The file event type"))
 ((name . "WatchKind") (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Create") (value . 1)
	    (documentation . "Interested in create events."))
	   ((name . "Change") (value . 2)
	    (documentation . "Interested in change events"))
	   ((name . "Delete") (value . 4)
	    (documentation . "Interested in delete events"))])
  (supportsCustomValues . t))
 ((name . "DiagnosticSeverity")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Error") (value . 1)
	    (documentation . "Reports an error."))
	   ((name . "Warning") (value . 2)
	    (documentation . "Reports a warning."))
	   ((name . "Information") (value . 3)
	    (documentation . "Reports an information."))
	   ((name . "Hint") (value . 4)
	    (documentation . "Reports a hint."))])
  (documentation . "The diagnostic's severity."))
 ((name . "DiagnosticTag") (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Unnecessary") (value . 1)
	    (documentation .
			   "Unused or unnecessary code.\n\nClients are allowed to render diagnostics with this tag faded out instead of having\nan error squiggle."))
	   ((name . "Deprecated") (value . 2)
	    (documentation .
			   "Deprecated or obsolete code.\n\nClients are allowed to rendered diagnostics with this tag strike through."))])
  (documentation . "The diagnostic tags.\n\n@since 3.15.0")
  (since . "3.15.0"))
 ((name . "CompletionTriggerKind")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Invoked") (value . 1)
	    (documentation .
			   "Completion was triggered by typing an identifier (24x7 code\ncomplete), manual invocation (e.g Ctrl+Space) or via API."))
	   ((name . "TriggerCharacter") (value . 2)
	    (documentation .
			   "Completion was triggered by a trigger character specified by\nthe `triggerCharacters` properties of the `CompletionRegistrationOptions`."))
	   ((name . "TriggerForIncompleteCompletions") (value . 3)
	    (documentation .
			   "Completion was re-triggered as current completion list is incomplete"))])
  (documentation . "How a completion was triggered"))
 ((name . "SignatureHelpTriggerKind")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Invoked") (value . 1)
	    (documentation .
			   "Signature help was invoked manually by the user or by a command."))
	   ((name . "TriggerCharacter") (value . 2)
	    (documentation .
			   "Signature help was triggered by a trigger character."))
	   ((name . "ContentChange") (value . 3)
	    (documentation .
			   "Signature help was triggered by the cursor moving or by the document content changing."))])
  (documentation .
		 "How a signature help was triggered.\n\n@since 3.15.0")
  (since . "3.15.0"))
 ((name . "CodeActionTriggerKind")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Invoked") (value . 1)
	    (documentation .
			   "Code actions were explicitly requested by the user or by an extension."))
	   ((name . "Automatic") (value . 2)
	    (documentation .
			   "Code actions were requested automatically.\n\nThis typically happens when current selection in a file changes, but can\nalso be triggered when file content changes."))])
  (documentation .
		 "The reason why code actions were requested.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "FileOperationPatternKind")
  (type (kind . "base") (name . "string"))
  (values .
	  [((name . "file") (value . "file")
	    (documentation . "The pattern matches a file only."))
	   ((name . "folder") (value . "folder")
	    (documentation . "The pattern matches a folder only."))])
  (documentation .
		 "A pattern kind describing if a glob pattern matches a file a folder or\nboth.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "NotebookCellKind")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Markup") (value . 1)
	    (documentation .
			   "A markup-cell is formatted source that is used for display."))
	   ((name . "Code") (value . 2)
	    (documentation . "A code-cell is source code."))])
  (documentation . "A notebook cell kind.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "ResourceOperationKind")
  (type (kind . "base") (name . "string"))
  (values .
	  [((name . "Create") (value . "create")
	    (documentation .
			   "Supports creating new files and folders."))
	   ((name . "Rename") (value . "rename")
	    (documentation .
			   "Supports renaming existing files and folders."))
	   ((name . "Delete") (value . "delete")
	    (documentation .
			   "Supports deleting existing files and folders."))]))
 ((name . "FailureHandlingKind")
  (type (kind . "base") (name . "string"))
  (values .
	  [((name . "Abort") (value . "abort")
	    (documentation .
			   "Applying the workspace change is simply aborted if one of the changes provided\nfails. All operations executed before the failing operation stay executed."))
	   ((name . "Transactional") (value . "transactional")
	    (documentation .
			   "All operations are executed transactional. That means they either all\nsucceed or no changes at all are applied to the workspace."))
	   ((name . "TextOnlyTransactional")
	    (value . "textOnlyTransactional")
	    (documentation .
			   "If the workspace edit contains only textual file changes they are executed transactional.\nIf resource changes (create, rename or delete file) are part of the change the failure\nhandling strategy is abort."))
	   ((name . "Undo") (value . "undo")
	    (documentation .
			   "The client tries to undo the operations already executed. But there is no\nguarantee that this is succeeding."))]))
 ((name . "PrepareSupportDefaultBehavior")
  (type (kind . "base") (name . "uinteger"))
  (values .
	  [((name . "Identifier") (value . 1)
	    (documentation .
			   "The client's default behavior is to select the identifier\naccording the to language's syntax rule."))]))
 ((name . "TokenFormat") (type (kind . "base") (name . "string"))
  (values . [((name . "Relative") (value . "relative"))]))]
