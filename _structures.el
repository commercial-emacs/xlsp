;; -*- lexical-binding: t -*-

[((name . "ImplementationParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))]))
 ((name . "Location")
  (properties .
	      [((name . "uri")
		(type (kind . "base") (name . "DocumentUri")))
	       ((name . "range")
		(type (kind . "reference") (name . "Range")))])
  (documentation .
		 "Represents a location inside a resource, such as a line\ninside a text file."))
 ((name . "ImplementationRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "ImplementationOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))]))
 ((name . "TypeDefinitionParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))]))
 ((name . "TypeDefinitionRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "TypeDefinitionOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))]))
 ((name . "WorkspaceFolder")
  (properties .
	      [((name . "uri") (type (kind . "base") (name . "URI"))
		(documentation .
			       "The associated URI for this workspace folder."))
	       ((name . "name")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The name of the workspace folder. Used to refer to this\nworkspace folder in the user interface."))])
  (documentation . "A workspace folder inside a client."))
 ((name . "DidChangeWorkspaceFoldersParams")
  (properties .
	      [((name . "event")
		(type (kind . "reference")
		      (name . "WorkspaceFoldersChangeEvent"))
		(documentation .
			       "The actual workspace folder change event."))])
  (documentation .
		 "The parameters of a `workspace/didChangeWorkspaceFolders` notification."))
 ((name . "ConfigurationParams")
  (properties .
	      [((name . "items")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "ConfigurationItem"))))])
  (documentation . "The parameters of a configuration request."))
 ((name . "PartialResultParams")
  (properties .
	      [((name . "partialResultToken")
		(type (kind . "reference") (name . "ProgressToken"))
		(optional . t)
		(documentation .
			       "An optional token that a server can use to report partial results (e.g. streaming) to\nthe client."))]))
 ((name . "DocumentColorParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "Parameters for a [DocumentColorRequest](#DocumentColorRequest)."))
 ((name . "ColorInformation")
  (properties .
	      [((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range in the document where this color appears."))
	       ((name . "color")
		(type (kind . "reference") (name . "Color"))
		(documentation .
			       "The actual color value for this color range."))])
  (documentation . "Represents a color range from a document."))
 ((name . "DocumentColorRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "DocumentColorOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))]))
 ((name . "ColorPresentationParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))
	       ((name . "color")
		(type (kind . "reference") (name . "Color"))
		(documentation .
			       "The color to request presentations for."))
	       ((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range where the color would be inserted. Serves as a context."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "Parameters for a [ColorPresentationRequest](#ColorPresentationRequest)."))
 ((name . "ColorPresentation")
  (properties .
	      [((name . "label")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The label of this color presentation. It will be shown on the color\npicker header. By default this is also the text that is inserted when selecting\nthis color presentation."))
	       ((name . "textEdit")
		(type (kind . "reference") (name . "TextEdit"))
		(optional . t)
		(documentation .
			       "An [edit](#TextEdit) which is applied to a document when selecting\nthis presentation for the color.  When `falsy` the [label](#ColorPresentation.label)\nis used."))
	       ((name . "additionalTextEdits")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "TextEdit")))
		(optional . t)
		(documentation .
			       "An optional array of additional [text edits](#TextEdit) that are applied when\nselecting this color presentation. Edits must not overlap with the main [edit](#ColorPresentation.textEdit) nor with themselves."))]))
 ((name . "WorkDoneProgressOptions")
  (properties .
	      [((name . "workDoneProgress")
		(type (kind . "base") (name . "boolean"))
		(optional . t))]))
 ((name . "TextDocumentRegistrationOptions")
  (properties .
	      [((name . "documentSelector")
		(type (kind . "or")
		      (items .
			     [((kind . "reference")
			       (name . "DocumentSelector"))
			      ((kind . "base") (name . "null"))]))
		(documentation .
			       "A document selector to identify the scope of the registration. If set to null\nthe document selector provided on the client side will be used."))])
  (documentation . "General text document registration options."))
 ((name . "FoldingRangeParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "Parameters for a [FoldingRangeRequest](#FoldingRangeRequest)."))
 ((name . "FoldingRange")
  (properties .
	      [((name . "startLine")
		(type (kind . "base") (name . "uinteger"))
		(documentation .
			       "The zero-based start line of the range to fold. The folded area starts after the line's last character.\nTo be valid, the end must be zero or larger and smaller than the number of lines in the document."))
	       ((name . "startCharacter")
		(type (kind . "base") (name . "uinteger"))
		(optional . t)
		(documentation .
			       "The zero-based character offset from where the folded range starts. If not defined, defaults to the length of the start line."))
	       ((name . "endLine")
		(type (kind . "base") (name . "uinteger"))
		(documentation .
			       "The zero-based end line of the range to fold. The folded area ends with the line's last character.\nTo be valid, the end must be zero or larger and smaller than the number of lines in the document."))
	       ((name . "endCharacter")
		(type (kind . "base") (name . "uinteger"))
		(optional . t)
		(documentation .
			       "The zero-based character offset before the folded range ends. If not defined, defaults to the length of the end line."))
	       ((name . "kind")
		(type (kind . "reference") (name . "FoldingRangeKind"))
		(optional . t)
		(documentation .
			       "Describes the kind of the folding range such as `comment' or 'region'. The kind\nis used to categorize folding ranges and used by commands like 'Fold all comments'.\nSee [FoldingRangeKind](#FoldingRangeKind) for an enumeration of standardized kinds."))
	       ((name . "collapsedText")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The text that the client should show when the specified range is\ncollapsed. If not defined or not supported by the client, a default\nwill be chosen by the client.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (documentation .
		 "Represents a folding range. To be valid, start and end line must be bigger than zero and smaller\nthan the number of lines in the document. Clients are free to ignore invalid ranges."))
 ((name . "FoldingRangeRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "FoldingRangeOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))]))
 ((name . "DeclarationParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))]))
 ((name . "DeclarationRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference") (name . "DeclarationOptions"))
	    ((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))]))
 ((name . "SelectionRangeParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))
	       ((name . "positions")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "Position")))
		(documentation .
			       "The positions inside the text document."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "A parameter literal used in selection range requests."))
 ((name . "SelectionRange")
  (properties .
	      [((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The [range](#Range) of this selection range."))
	       ((name . "parent")
		(type (kind . "reference") (name . "SelectionRange"))
		(optional . t)
		(documentation .
			       "The parent selection range containing this range. Therefore `parent.range` must contain `this.range`."))])
  (documentation .
		 "A selection range represents a part of a selection hierarchy. A selection range\nmay have a parent selection range that contains it."))
 ((name . "SelectionRangeRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference") (name . "SelectionRangeOptions"))
	    ((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))]))
 ((name . "WorkDoneProgressCreateParams")
  (properties .
	      [((name . "token")
		(type (kind . "reference") (name . "ProgressToken"))
		(documentation .
			       "The token to be used to report progress."))]))
 ((name . "WorkDoneProgressCancelParams")
  (properties .
	      [((name . "token")
		(type (kind . "reference") (name . "ProgressToken"))
		(documentation .
			       "The token to be used to report progress."))]))
 ((name . "CallHierarchyPrepareParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation .
		 "The parameter of a `textDocument/prepareCallHierarchy` request.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "CallHierarchyItem")
  (properties .
	      [((name . "name")
		(type (kind . "base") (name . "string"))
		(documentation . "The name of this item."))
	       ((name . "kind")
		(type (kind . "reference") (name . "SymbolKind"))
		(documentation . "The kind of this item."))
	       ((name . "tags")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "SymbolTag")))
		(optional . t) (documentation . "Tags for this item."))
	       ((name . "detail")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "More detail for this item, e.g. the signature of a function."))
	       ((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation .
			       "The resource identifier of this item."))
	       ((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range enclosing this symbol not including leading/trailing whitespace but everything else, e.g. comments and code."))
	       ((name . "selectionRange")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range that should be selected and revealed when this symbol is being picked, e.g. the name of a function.\nMust be contained by the [`range`](#CallHierarchyItem.range)."))
	       ((name . "data")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "A data entry field that is preserved between a call hierarchy prepare and\nincoming calls or outgoing calls requests."))])
  (documentation .
		 "Represents programming constructs like functions or constructors in the context\nof call hierarchy.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "CallHierarchyRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "CallHierarchyOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))])
  (documentation .
		 "Call hierarchy options used during static or dynamic registration.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "CallHierarchyIncomingCallsParams")
  (properties .
	      [((name . "item")
		(type (kind . "reference")
		      (name . "CallHierarchyItem")))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "The parameter of a `callHierarchy/incomingCalls` request.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "CallHierarchyIncomingCall")
  (properties .
	      [((name . "from")
		(type (kind . "reference")
		      (name . "CallHierarchyItem"))
		(documentation . "The item that makes the call."))
	       ((name . "fromRanges")
		(type (kind . "array")
		      (element (kind . "reference") (name . "Range")))
		(documentation .
			       "The ranges at which the calls appear. This is relative to the caller\ndenoted by [`this.from`](#CallHierarchyIncomingCall.from)."))])
  (documentation .
		 "Represents an incoming call, e.g. a caller of a method or constructor.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "CallHierarchyOutgoingCallsParams")
  (properties .
	      [((name . "item")
		(type (kind . "reference")
		      (name . "CallHierarchyItem")))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "The parameter of a `callHierarchy/outgoingCalls` request.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "CallHierarchyOutgoingCall")
  (properties .
	      [((name . "to")
		(type (kind . "reference")
		      (name . "CallHierarchyItem"))
		(documentation . "The item that is called."))
	       ((name . "fromRanges")
		(type (kind . "array")
		      (element (kind . "reference") (name . "Range")))
		(documentation .
			       "The range at which this item is called. This is the range relative to the caller, e.g the item\npassed to [`provideCallHierarchyOutgoingCalls`](#CallHierarchyItemProvider.provideCallHierarchyOutgoingCalls)\nand not [`this.to`](#CallHierarchyOutgoingCall.to)."))])
  (documentation .
		 "Represents an outgoing call, e.g. calling a getter from a method or a method from a constructor etc.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "SemanticTokensParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "SemanticTokens")
  (properties .
	      [((name . "resultId")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "An optional result id. If provided and clients support delta updating\nthe client will include the result id in the next semantic token request.\nA server can then instead of computing all semantic tokens again simply\nsend a delta."))
	       ((name . "data")
		(type (kind . "array")
		      (element (kind . "base") (name . "uinteger")))
		(documentation . "The actual tokens."))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "SemanticTokensPartialResult")
  (properties .
	      [((name . "data")
		(type (kind . "array")
		      (element (kind . "base") (name . "uinteger"))))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "SemanticTokensRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "SemanticTokensOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "SemanticTokensDeltaParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))
	       ((name . "previousResultId")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The result id of a previous response. The result Id can either point to a full response\nor a delta response depending on what was received last."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "SemanticTokensDelta")
  (properties .
	      [((name . "resultId")
		(type (kind . "base") (name . "string"))
		(optional . t))
	       ((name . "edits")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "SemanticTokensEdit")))
		(documentation .
			       "The semantic token edits to transform a previous result into a new result."))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "SemanticTokensDeltaPartialResult")
  (properties .
	      [((name . "edits")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "SemanticTokensEdit"))))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "SemanticTokensRangeParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))
	       ((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range the semantic tokens are requested for."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "ShowDocumentParams")
  (properties .
	      [((name . "uri") (type (kind . "base") (name . "URI"))
		(documentation . "The document uri to show."))
	       ((name . "external")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Indicates to show the resource in an external program.\nTo show for example `https://code.visualstudio.com/`\nin the default WEB browser set `external` to `true`."))
	       ((name . "takeFocus")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "An optional property to indicate whether the editor\nshowing the document should take focus or not.\nClients might ignore this property if an external\nprogram is started."))
	       ((name . "selection")
		(type (kind . "reference") (name . "Range"))
		(optional . t)
		(documentation .
			       "An optional selection range if the document is a text\ndocument. Clients might ignore the property if an\nexternal program is started or the file is not a text\nfile."))])
  (documentation . "Params to show a document.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "ShowDocumentResult")
  (properties .
	      [((name . "success")
		(type (kind . "base") (name . "boolean"))
		(documentation .
			       "A boolean indicating if the show was successful."))])
  (documentation .
		 "The result of a showDocument request.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "LinkedEditingRangeParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))]))
 ((name . "LinkedEditingRanges")
  (properties .
	      [((name . "ranges")
		(type (kind . "array")
		      (element (kind . "reference") (name . "Range")))
		(documentation .
			       "A list of ranges that can be edited together. The ranges must have\nidentical length and contain identical text content. The ranges cannot overlap."))
	       ((name . "wordPattern")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "An optional word pattern (regular expression) that describes valid contents for\nthe given ranges. If no pattern is provided, the client configuration's word\npattern will be used."))])
  (documentation .
		 "The result of a linked editing range request.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "LinkedEditingRangeRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "LinkedEditingRangeOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))]))
 ((name . "CreateFilesParams")
  (properties .
	      [((name . "files")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "FileCreate")))
		(documentation .
			       "An array of all files/folders created in this operation."))])
  (documentation .
		 "The parameters sent in notifications/requests for user-initiated creation of\nfiles.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "WorkspaceEdit")
  (properties .
	      [((name . "changes")
		(type (kind . "map")
		      (key (kind . "base") (name . "DocumentUri"))
		      (value (kind . "array")
			     (element (kind . "reference")
				      (name . "TextEdit"))))
		(optional . t)
		(documentation .
			       "Holds changes to existing resources."))
	       ((name . "documentChanges")
		(type (kind . "array")
		      (element (kind . "or")
			       (items .
				      [((kind . "reference")
					(name . "TextDocumentEdit"))
				       ((kind . "reference")
					(name . "CreateFile"))
				       ((kind . "reference")
					(name . "RenameFile"))
				       ((kind . "reference")
					(name . "DeleteFile"))])))
		(optional . t)
		(documentation .
			       "Depending on the client capability `workspace.workspaceEdit.resourceOperations` document changes\nare either an array of `TextDocumentEdit`s to express changes to n different text documents\nwhere each text document edit addresses a specific version of a text document. Or it can contain\nabove `TextDocumentEdit`s mixed with create, rename and delete file / folder operations.\n\nWhether a client supports versioned document edits is expressed via\n`workspace.workspaceEdit.documentChanges` client capability.\n\nIf a client neither supports `documentChanges` nor `workspace.workspaceEdit.resourceOperations` then\nonly plain `TextEdit`s using the `changes` property are supported."))
	       ((name . "changeAnnotations")
		(type (kind . "map")
		      (key (kind . "reference")
			   (name . "ChangeAnnotationIdentifier"))
		      (value (kind . "reference")
			     (name . "ChangeAnnotation")))
		(optional . t)
		(documentation .
			       "A map of change annotations that can be referenced in `AnnotatedTextEdit`s or create, rename and\ndelete file / folder operations.\n\nWhether clients honor this property depends on the client capability `workspace.changeAnnotationSupport`.\n\n@since 3.16.0")
		(since . "3.16.0"))])
  (documentation .
		 "A workspace edit represents changes to many resources managed in the workspace. The edit\nshould either provide `changes` or `documentChanges`. If documentChanges are present\nthey are preferred over `changes` if the client can handle versioned document edits.\n\nSince version 3.13.0 a workspace edit can contain resource operations as well. If resource\noperations are present clients need to execute the operations in the order in which they\nare provided. So a workspace edit for example can consist of the following two changes:\n(1) a create file a.txt and (2) a text document edit which insert text into file a.txt.\n\nAn invalid sequence (e.g. (1) delete file a.txt and (2) insert text into file a.txt) will\ncause failure of the operation. How the client recovers from the failure is described by\nthe client capability: `workspace.workspaceEdit.failureHandling`"))
 ((name . "FileOperationRegistrationOptions")
  (properties .
	      [((name . "filters")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "FileOperationFilter")))
		(documentation . "The actual filters."))])
  (documentation .
		 "The options to register for file operations.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "RenameFilesParams")
  (properties .
	      [((name . "files")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "FileRename")))
		(documentation .
			       "An array of all files/folders renamed in this operation. When a folder is renamed, only\nthe folder will be included, and not its children."))])
  (documentation .
		 "The parameters sent in notifications/requests for user-initiated renames of\nfiles.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "DeleteFilesParams")
  (properties .
	      [((name . "files")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "FileDelete")))
		(documentation .
			       "An array of all files/folders deleted in this operation."))])
  (documentation .
		 "The parameters sent in notifications/requests for user-initiated deletes of\nfiles.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "MonikerParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))]))
 ((name . "Moniker")
  (properties .
	      [((name . "scheme")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The scheme of the moniker. For example tsc or .Net"))
	       ((name . "identifier")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The identifier of the moniker. The value is opaque in LSIF however\nschema owners are allowed to define the structure if they want."))
	       ((name . "unique")
		(type (kind . "reference") (name . "UniquenessLevel"))
		(documentation .
			       "The scope in which the moniker is unique"))
	       ((name . "kind")
		(type (kind . "reference") (name . "MonikerKind"))
		(optional . t)
		(documentation . "The moniker kind if known."))])
  (documentation .
		 "Moniker definition to match LSIF 0.5 moniker definition.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "MonikerRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "MonikerOptions"))]))
 ((name . "TypeHierarchyPrepareParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation .
		 "The parameter of a `textDocument/prepareTypeHierarchy` request.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "TypeHierarchyItem")
  (properties .
	      [((name . "name")
		(type (kind . "base") (name . "string"))
		(documentation . "The name of this item."))
	       ((name . "kind")
		(type (kind . "reference") (name . "SymbolKind"))
		(documentation . "The kind of this item."))
	       ((name . "tags")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "SymbolTag")))
		(optional . t) (documentation . "Tags for this item."))
	       ((name . "detail")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "More detail for this item, e.g. the signature of a function."))
	       ((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation .
			       "The resource identifier of this item."))
	       ((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range enclosing this symbol not including leading/trailing whitespace\nbut everything else, e.g. comments and code."))
	       ((name . "selectionRange")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range that should be selected and revealed when this symbol is being\npicked, e.g. the name of a function. Must be contained by the\n[`range`](#TypeHierarchyItem.range)."))
	       ((name . "data")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "A data entry field that is preserved between a type hierarchy prepare and\nsupertypes or subtypes requests. It could also be used to identify the\ntype hierarchy in the server, helping improve the performance on\nresolving supertypes and subtypes."))])
  (documentation . "@since 3.17.0") (since . "3.17.0"))
 ((name . "TypeHierarchyRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "TypeHierarchyOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))])
  (documentation .
		 "Type hierarchy options used during static or dynamic registration.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "TypeHierarchySupertypesParams")
  (properties .
	      [((name . "item")
		(type (kind . "reference")
		      (name . "TypeHierarchyItem")))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "The parameter of a `typeHierarchy/supertypes` request.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "TypeHierarchySubtypesParams")
  (properties .
	      [((name . "item")
		(type (kind . "reference")
		      (name . "TypeHierarchyItem")))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "The parameter of a `typeHierarchy/subtypes` request.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlineValueParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))
	       ((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The document range for which inline values should be computed."))
	       ((name . "context")
		(type (kind . "reference")
		      (name . "InlineValueContext"))
		(documentation .
			       "Additional information about the context in which inline values were\nrequested."))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation .
		 "A parameter literal used in inline value requests.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlineValueRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference") (name . "InlineValueOptions"))
	    ((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))])
  (documentation .
		 "Inline value options used during static or dynamic registration.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlayHintParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))
	       ((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The document range for which inlay hints should be computed."))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation .
		 "A parameter literal used in inlay hint requests.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlayHint")
  (properties .
	      [((name . "position")
		(type (kind . "reference") (name . "Position"))
		(documentation . "The position of this hint."))
	       ((name . "label")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "array")
			       (element (kind . "reference")
					(name . "InlayHintLabelPart")))]))
		(documentation .
			       "The label of this hint. A human readable string or an array of\nInlayHintLabelPart label parts.\n\n*Note* that neither the string nor the label part can be empty."))
	       ((name . "kind")
		(type (kind . "reference") (name . "InlayHintKind"))
		(optional . t)
		(documentation .
			       "The kind of this hint. Can be omitted in which case the client\nshould fall back to a reasonable default."))
	       ((name . "textEdits")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "TextEdit")))
		(optional . t)
		(documentation .
			       "Optional text edits that are performed when accepting this inlay hint.\n\n*Note* that edits are expected to change the document so that the inlay\nhint (or its nearest variant) is now part of the document and the inlay\nhint itself is now obsolete."))
	       ((name . "tooltip")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "reference")
			       (name . "MarkupContent"))]))
		(optional . t)
		(documentation .
			       "The tooltip text when you hover over this item."))
	       ((name . "paddingLeft")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Render padding before the hint.\n\nNote: Padding should use the editor's background color, not the\nbackground color of the hint itself. That means padding can be used\nto visually align/separate an inlay hint."))
	       ((name . "paddingRight")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Render padding after the hint.\n\nNote: Padding should use the editor's background color, not the\nbackground color of the hint itself. That means padding can be used\nto visually align/separate an inlay hint."))
	       ((name . "data")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "A data entry field that is preserved on an inlay hint between\na `textDocument/inlayHint` and a `inlayHint/resolve` request."))])
  (documentation . "Inlay hint information.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlayHintRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference") (name . "InlayHintOptions"))
	    ((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))])
  (documentation .
		 "Inlay hint options used during static or dynamic registration.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DocumentDiagnosticParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))
	       ((name . "identifier")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The additional identifier  provided during registration."))
	       ((name . "previousResultId")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The result id of a previous response if provided."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "Parameters of the document diagnostic request.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DocumentDiagnosticReportPartialResult")
  (properties .
	      [((name . "relatedDocuments")
		(type (kind . "map")
		      (key (kind . "base") (name . "DocumentUri"))
		      (value (kind . "or")
			     (items .
				    [((kind . "reference")
				      (name .
					    "FullDocumentDiagnosticReport"))
				     ((kind . "reference")
				      (name .
					    "UnchangedDocumentDiagnosticReport"))]))))])
  (documentation .
		 "A partial result for a document diagnostic report.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DiagnosticServerCancellationData")
  (properties .
	      [((name . "retriggerRequest")
		(type (kind . "base") (name . "boolean")))])
  (documentation .
		 "Cancellation data returned from a diagnostic request.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DiagnosticRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "DiagnosticOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))])
  (documentation . "Diagnostic registration options.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "WorkspaceDiagnosticParams")
  (properties .
	      [((name . "identifier")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The additional identifier provided during registration."))
	       ((name . "previousResultIds")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "PreviousResultId")))
		(documentation .
			       "The currently known diagnostic reports with their\nprevious result ids."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "Parameters of the workspace diagnostic request.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "WorkspaceDiagnosticReport")
  (properties .
	      [((name . "items")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name .
				     "WorkspaceDocumentDiagnosticReport"))))])
  (documentation . "A workspace diagnostic report.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "WorkspaceDiagnosticReportPartialResult")
  (properties .
	      [((name . "items")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name .
				     "WorkspaceDocumentDiagnosticReport"))))])
  (documentation .
		 "A partial result for a workspace diagnostic report.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DidOpenNotebookDocumentParams")
  (properties .
	      [((name . "notebookDocument")
		(type (kind . "reference") (name . "NotebookDocument"))
		(documentation .
			       "The notebook document that got opened."))
	       ((name . "cellTextDocuments")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "TextDocumentItem")))
		(documentation .
			       "The text documents that represent the content\nof a notebook cell."))])
  (documentation .
		 "The params sent in an open notebook document notification.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DidChangeNotebookDocumentParams")
  (properties .
	      [((name . "notebookDocument")
		(type (kind . "reference")
		      (name . "VersionedNotebookDocumentIdentifier"))
		(documentation .
			       "The notebook document that did change. The version number points\nto the version after all provided changes have been applied. If\nonly the text document content of a cell changes the notebook version\ndoesn't necessarily have to change."))
	       ((name . "change")
		(type (kind . "reference")
		      (name . "NotebookDocumentChangeEvent"))
		(documentation .
			       "The actual changes to the notebook document.\n\nThe changes describe single state changes to the notebook document.\nSo if there are two changes c1 (at array index 0) and c2 (at array\nindex 1) for a notebook in state S then c1 moves the notebook from\nS to S' and c2 from S' to S''. So c1 is computed on the state S and\nc2 is computed on the state S'.\n\nTo mirror the content of a notebook using change events use the following approach:\n- start with the same initial content\n- apply the 'notebookDocument/didChange' notifications in the order you receive them.\n- apply the `NotebookChangeEvent`s in a single notification in the order\n  you receive them."))])
  (documentation .
		 "The params sent in a change notebook document notification.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DidSaveNotebookDocumentParams")
  (properties .
	      [((name . "notebookDocument")
		(type (kind . "reference")
		      (name . "NotebookDocumentIdentifier"))
		(documentation .
			       "The notebook document that got saved."))])
  (documentation .
		 "The params sent in a save notebook document notification.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DidCloseNotebookDocumentParams")
  (properties .
	      [((name . "notebookDocument")
		(type (kind . "reference")
		      (name . "NotebookDocumentIdentifier"))
		(documentation .
			       "The notebook document that got closed."))
	       ((name . "cellTextDocuments")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "TextDocumentIdentifier")))
		(documentation .
			       "The text documents that represent the content\nof a notebook cell that got closed."))])
  (documentation .
		 "The params sent in a close notebook document notification.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "RegistrationParams")
  (properties .
	      [((name . "registrations")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "Registration"))))]))
 ((name . "UnregistrationParams")
  (properties .
	      [((name . "unregisterations")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "Unregistration"))))]))
 ((name . "InitializeParams") (properties . [])
  (extends .
	   [((kind . "reference") (name . "_InitializeParams"))
	    ((kind . "reference")
	     (name . "WorkspaceFoldersInitializeParams"))]))
 ((name . "InitializeResult")
  (properties .
	      [((name . "capabilities")
		(type (kind . "reference")
		      (name . "ServerCapabilities"))
		(documentation .
			       "The capabilities the language server provides."))
	       ((name . "serverInfo")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "name")
				     (type (kind . "base")
					   (name . "string"))
				     (documentation .
						    "The name of the server as defined by the server."))
				    ((name . "version")
				     (type (kind . "base")
					   (name . "string"))
				     (optional . t)
				     (documentation .
						    "The server's version as defined by the server."))])))
		(optional . t)
		(documentation .
			       "Information about the server.\n\n@since 3.15.0")
		(since . "3.15.0"))])
  (documentation . "The result returned from an initialize request."))
 ((name . "InitializeError")
  (properties .
	      [((name . "retry")
		(type (kind . "base") (name . "boolean"))
		(documentation .
			       "Indicates whether the client execute the following retry logic:\n(1) show the message provided by the ResponseError to the user\n(2) user selects retry or cancel\n(3) if user selected retry the initialize method is sent again."))])
  (documentation .
		 "The data type of the ResponseError if the\ninitialize request fails."))
 ((name . "InitializedParams") (properties . []))
 ((name . "DidChangeConfigurationParams")
  (properties .
	      [((name . "settings")
		(type (kind . "reference") (name . "LSPAny"))
		(documentation . "The actual changed settings"))])
  (documentation .
		 "The parameters of a change configuration notification."))
 ((name . "DidChangeConfigurationRegistrationOptions")
  (properties .
	      [((name . "section")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "array")
			       (element (kind . "base")
					(name . "string")))]))
		(optional . t))]))
 ((name . "ShowMessageParams")
  (properties .
	      [((name . "type")
		(type (kind . "reference") (name . "MessageType"))
		(documentation .
			       "The message type. See {@link MessageType}"))
	       ((name . "message")
		(type (kind . "base") (name . "string"))
		(documentation . "The actual message."))])
  (documentation . "The parameters of a notification message."))
 ((name . "ShowMessageRequestParams")
  (properties .
	      [((name . "type")
		(type (kind . "reference") (name . "MessageType"))
		(documentation .
			       "The message type. See {@link MessageType}"))
	       ((name . "message")
		(type (kind . "base") (name . "string"))
		(documentation . "The actual message."))
	       ((name . "actions")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "MessageActionItem")))
		(optional . t)
		(documentation .
			       "The message action items to present."))]))
 ((name . "MessageActionItem")
  (properties .
	      [((name . "title")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "A short title like 'Retry', 'Open Log' etc."))]))
 ((name . "LogMessageParams")
  (properties .
	      [((name . "type")
		(type (kind . "reference") (name . "MessageType"))
		(documentation .
			       "The message type. See {@link MessageType}"))
	       ((name . "message")
		(type (kind . "base") (name . "string"))
		(documentation . "The actual message."))])
  (documentation . "The log message parameters."))
 ((name . "DidOpenTextDocumentParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference") (name . "TextDocumentItem"))
		(documentation . "The document that was opened."))])
  (documentation .
		 "The parameters sent in an open text document notification"))
 ((name . "DidChangeTextDocumentParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "VersionedTextDocumentIdentifier"))
		(documentation .
			       "The document that did change. The version number points\nto the version after all provided content changes have\nbeen applied."))
	       ((name . "contentChanges")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name .
				     "TextDocumentContentChangeEvent")))
		(documentation .
			       "The actual content changes. The content changes describe single state changes\nto the document. So if there are two content changes c1 (at array index 0) and\nc2 (at array index 1) for a document in state S then c1 moves the document from\nS to S' and c2 from S' to S''. So c1 is computed on the state S and c2 is computed\non the state S'.\n\nTo mirror the content of a document using change events use the following approach:\n- start with the same initial content\n- apply the 'textDocument/didChange' notifications in the order you receive them.\n- apply the `TextDocumentContentChangeEvent`s in a single notification in the order\n  you receive them."))])
  (documentation .
		 "The change text document notification's parameters."))
 ((name . "TextDocumentChangeRegistrationOptions")
  (properties .
	      [((name . "syncKind")
		(type (kind . "reference")
		      (name . "TextDocumentSyncKind"))
		(documentation .
			       "How documents are synced to the server."))])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))])
  (documentation .
		 "Describe options to be used when registered for text document change events."))
 ((name . "DidCloseTextDocumentParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The document that was closed."))])
  (documentation .
		 "The parameters sent in a close text document notification"))
 ((name . "DidSaveTextDocumentParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The document that was saved."))
	       ((name . "text")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "Optional the content when saved. Depends on the includeText value\nwhen the save notification was requested."))])
  (documentation .
		 "The parameters sent in a save text document notification"))
 ((name . "TextDocumentSaveRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "SaveOptions"))])
  (documentation . "Save registration options."))
 ((name . "WillSaveTextDocumentParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The document that will be saved."))
	       ((name . "reason")
		(type (kind . "reference")
		      (name . "TextDocumentSaveReason"))
		(documentation . "The 'TextDocumentSaveReason'."))])
  (documentation .
		 "The parameters sent in a will save text document notification."))
 ((name . "TextEdit")
  (properties .
	      [((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range of the text document to be manipulated. To insert\ntext into a document create a range where start === end."))
	       ((name . "newText")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The string to be inserted. For delete operations use an\nempty string."))])
  (documentation . "A text edit applicable to a text document."))
 ((name . "DidChangeWatchedFilesParams")
  (properties .
	      [((name . "changes")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "FileEvent")))
		(documentation . "The actual file events."))])
  (documentation .
		 "The watched files change notification's parameters."))
 ((name . "DidChangeWatchedFilesRegistrationOptions")
  (properties .
	      [((name . "watchers")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "FileSystemWatcher")))
		(documentation . "The watchers to register."))])
  (documentation .
		 "Describe options to be used when registered for text document change events."))
 ((name . "PublishDiagnosticsParams")
  (properties .
	      [((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation .
			       "The URI for which diagnostic information is reported."))
	       ((name . "version")
		(type (kind . "base") (name . "integer"))
		(optional . t)
		(documentation .
			       "Optional the version number of the document the diagnostics are published for.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "diagnostics")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "Diagnostic")))
		(documentation .
			       "An array of diagnostic information items."))])
  (documentation . "The publish diagnostic notification's parameters."))
 ((name . "CompletionParams")
  (properties .
	      [((name . "context")
		(type (kind . "reference")
		      (name . "CompletionContext"))
		(optional . t)
		(documentation .
			       "The completion context. This is only available it the client specifies\nto send this using the client capability `textDocument.completion.contextSupport === true`"))])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation . "Completion parameters"))
 ((name . "CompletionItem")
  (properties .
	      [((name . "label")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The label of this completion item.\n\nThe label property is also by default the text that\nis inserted when selecting this completion.\n\nIf label details are provided the label itself should\nbe an unqualified name of the completion item."))
	       ((name . "labelDetails")
		(type (kind . "reference")
		      (name . "CompletionItemLabelDetails"))
		(optional . t)
		(documentation .
			       "Additional details for the label\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "kind")
		(type (kind . "reference")
		      (name . "CompletionItemKind"))
		(optional . t)
		(documentation .
			       "The kind of this completion item. Based of the kind\nan icon is chosen by the editor."))
	       ((name . "tags")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "CompletionItemTag")))
		(optional . t)
		(documentation .
			       "Tags for this completion item.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "detail")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "A human-readable string with additional information\nabout this item, like type or symbol information."))
	       ((name . "documentation")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "reference")
			       (name . "MarkupContent"))]))
		(optional . t)
		(documentation .
			       "A human-readable string that represents a doc-comment."))
	       ((name . "deprecated")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Indicates if this item is deprecated.\n@deprecated Use `tags` instead."))
	       ((name . "preselect")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Select this item when showing.\n\n*Note* that only one completion item can be selected and that the\ntool / client decides which item that is. The rule is that the *first*\nitem of those that match best is selected."))
	       ((name . "sortText")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "A string that should be used when comparing this item\nwith other items. When `falsy` the [label](#CompletionItem.label)\nis used."))
	       ((name . "filterText")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "A string that should be used when filtering a set of\ncompletion items. When `falsy` the [label](#CompletionItem.label)\nis used."))
	       ((name . "insertText")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "A string that should be inserted into a document when selecting\nthis completion. When `falsy` the [label](#CompletionItem.label)\nis used.\n\nThe `insertText` is subject to interpretation by the client side.\nSome tools might not take the string literally. For example\nVS Code when code complete is requested in this example\n`con<cursor position>` and a completion item with an `insertText` of\n`console` is provided it will only insert `sole`. Therefore it is\nrecommended to use `textEdit` instead since it avoids additional client\nside interpretation."))
	       ((name . "insertTextFormat")
		(type (kind . "reference") (name . "InsertTextFormat"))
		(optional . t)
		(documentation .
			       "The format of the insert text. The format applies to both the\n`insertText` property and the `newText` property of a provided\n`textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.\n\nPlease note that the insertTextFormat doesn't apply to\n`additionalTextEdits`."))
	       ((name . "insertTextMode")
		(type (kind . "reference") (name . "InsertTextMode"))
		(optional . t)
		(documentation .
			       "How whitespace and indentation is handled during completion\nitem insertion. If not provided the clients default value depends on\nthe `textDocument.completion.insertTextMode` client capability.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "textEdit")
		(type (kind . "or")
		      (items .
			     [((kind . "reference")
			       (name . "TextEdit"))
			      ((kind . "reference")
			       (name . "InsertReplaceEdit"))]))
		(optional . t)
		(documentation .
			       "An [edit](#TextEdit) which is applied to a document when selecting\nthis completion. When an edit is provided the value of\n[insertText](#CompletionItem.insertText) is ignored.\n\nMost editors support two different operations when accepting a completion\nitem. One is to insert a completion text and the other is to replace an\nexisting text with a completion text. Since this can usually not be\npredetermined by a server it can report both ranges. Clients need to\nsignal support for `InsertReplaceEdits` via the\n`textDocument.completion.insertReplaceSupport` client capability\nproperty.\n\n*Note 1:* The text edit's range as well as both ranges from an insert\nreplace edit must be a [single line] and they must contain the position\nat which completion has been requested.\n*Note 2:* If an `InsertReplaceEdit` is returned the edit's insert range\nmust be a prefix of the edit's replace range, that means it must be\ncontained and starting at the same position.\n\n@since 3.16.0 additional type `InsertReplaceEdit`")
		(since . "3.16.0 additional type `InsertReplaceEdit`"))
	       ((name . "textEditText")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The edit text used if the completion item is part of a CompletionList and\nCompletionList defines an item default for the text edit range.\n\nClients will only honor this property if they opt into completion list\nitem defaults using the capability `completionList.itemDefaults`.\n\nIf not provided and a list's default range is provided the label\nproperty is used as a text.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "additionalTextEdits")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "TextEdit")))
		(optional . t)
		(documentation .
			       "An optional array of additional [text edits](#TextEdit) that are applied when\nselecting this completion. Edits must not overlap (including the same insert position)\nwith the main [edit](#CompletionItem.textEdit) nor with themselves.\n\nAdditional text edits should be used to change text unrelated to the current cursor position\n(for example adding an import statement at the top of the file if the completion item will\ninsert an unqualified type)."))
	       ((name . "commitCharacters")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(optional . t)
		(documentation .
			       "An optional set of characters that when pressed while this completion is active will accept it first and\nthen type that character. *Note* that all commit characters should have `length=1` and that superfluous\ncharacters will be ignored."))
	       ((name . "command")
		(type (kind . "reference") (name . "Command"))
		(optional . t)
		(documentation .
			       "An optional [command](#Command) that is executed *after* inserting this completion. *Note* that\nadditional modifications to the current document should be described with the\n[additionalTextEdits](#CompletionItem.additionalTextEdits)-property."))
	       ((name . "data")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "A data entry field that is preserved on a completion item between a\n[CompletionRequest](#CompletionRequest) and a [CompletionResolveRequest](#CompletionResolveRequest)."))])
  (documentation .
		 "A completion item represents a text snippet that is\nproposed to complete text that is being typed."))
 ((name . "CompletionList")
  (properties .
	      [((name . "isIncomplete")
		(type (kind . "base") (name . "boolean"))
		(documentation .
			       "This list it not complete. Further typing results in recomputing this list.\n\nRecomputed lists have all their items replaced (not appended) in the\nincomplete completion sessions."))
	       ((name . "itemDefaults")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "commitCharacters")
				     (type (kind . "array")
					   (element (kind . "base")
						    (name . "string")))
				     (optional . t)
				     (documentation .
						    "A default commit character set.\n\n@since 3.17.0")
				     (since . "3.17.0"))
				    ((name . "editRange")
				     (type (kind . "or")
					   (items .
						  [((kind .
							  "reference")
						    (name . "Range"))
						   ((kind . "literal")
						    (value
						     (properties .
								 [((name
								    .
								    "insert")
								   (type
								    (kind
								     .
								     "reference")
								    (name
								     .
								     "Range")))
								  ((name
								    .
								    "replace")
								   (type
								    (kind
								     .
								     "reference")
								    (name
								     .
								     "Range")))])))]))
				     (optional . t)
				     (documentation .
						    "A default edit range.\n\n@since 3.17.0")
				     (since . "3.17.0"))
				    ((name . "insertTextFormat")
				     (type (kind . "reference")
					   (name . "InsertTextFormat"))
				     (optional . t)
				     (documentation .
						    "A default insert text format.\n\n@since 3.17.0")
				     (since . "3.17.0"))
				    ((name . "insertTextMode")
				     (type (kind . "reference")
					   (name . "InsertTextMode"))
				     (optional . t)
				     (documentation .
						    "A default insert text mode.\n\n@since 3.17.0")
				     (since . "3.17.0"))
				    ((name . "data")
				     (type (kind . "reference")
					   (name . "LSPAny"))
				     (optional . t)
				     (documentation .
						    "A default data value.\n\n@since 3.17.0")
				     (since . "3.17.0"))])))
		(optional . t)
		(documentation .
			       "In many cases the items of an actual completion result share the same\nvalue for properties like `commitCharacters` or the range of a text\nedit. A completion list can therefore define item defaults which will\nbe used if a completion item itself doesn't specify the value.\n\nIf a completion list specifies a default value and a completion item\nalso specifies a corresponding value the one from the item is used.\n\nServers are only allowed to return default values if the client\nsignals support for this via the `completionList.itemDefaults`\ncapability.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "items")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "CompletionItem")))
		(documentation . "The completion items."))])
  (documentation .
		 "Represents a collection of [completion items](#CompletionItem) to be presented\nin the editor."))
 ((name . "CompletionRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "CompletionOptions"))])
  (documentation .
		 "Registration options for a [CompletionRequest](#CompletionRequest)."))
 ((name . "HoverParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation . "Parameters for a [HoverRequest](#HoverRequest)."))
 ((name . "Hover")
  (properties .
	      [((name . "contents")
		(type (kind . "or")
		      (items .
			     [((kind . "reference")
			       (name . "MarkupContent"))
			      ((kind . "reference")
			       (name . "MarkedString"))
			      ((kind . "array")
			       (element (kind . "reference")
					(name . "MarkedString")))]))
		(documentation . "The hover's content"))
	       ((name . "range")
		(type (kind . "reference") (name . "Range"))
		(optional . t)
		(documentation .
			       "An optional range inside the text document that is used to\nvisualize the hover, e.g. by changing the background color."))])
  (documentation . "The result of a hover request."))
 ((name . "HoverRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "HoverOptions"))])
  (documentation .
		 "Registration options for a [HoverRequest](#HoverRequest)."))
 ((name . "SignatureHelpParams")
  (properties .
	      [((name . "context")
		(type (kind . "reference")
		      (name . "SignatureHelpContext"))
		(optional . t)
		(documentation .
			       "The signature help context. This is only available if the client specifies\nto send this using the client capability `textDocument.signatureHelp.contextSupport === true`\n\n@since 3.15.0")
		(since . "3.15.0"))])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation .
		 "Parameters for a [SignatureHelpRequest](#SignatureHelpRequest)."))
 ((name . "SignatureHelp")
  (properties .
	      [((name . "signatures")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "SignatureInformation")))
		(documentation . "One or more signatures."))
	       ((name . "activeSignature")
		(type (kind . "base") (name . "uinteger"))
		(optional . t)
		(documentation .
			       "The active signature. If omitted or the value lies outside the\nrange of `signatures` the value defaults to zero or is ignored if\nthe `SignatureHelp` has no signatures.\n\nWhenever possible implementors should make an active decision about\nthe active signature and shouldn't rely on a default value.\n\nIn future version of the protocol this property might become\nmandatory to better express this."))
	       ((name . "activeParameter")
		(type (kind . "base") (name . "uinteger"))
		(optional . t)
		(documentation .
			       "The active parameter of the active signature. If omitted or the value\nlies outside the range of `signatures[activeSignature].parameters`\ndefaults to 0 if the active signature has parameters. If\nthe active signature has no parameters it is ignored.\nIn future version of the protocol this property might become\nmandatory to better express the active parameter if the\nactive signature does have any."))])
  (documentation .
		 "Signature help represents the signature of something\ncallable. There can be multiple signature but only one\nactive and only one active parameter."))
 ((name . "SignatureHelpRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "SignatureHelpOptions"))])
  (documentation .
		 "Registration options for a [SignatureHelpRequest](#SignatureHelpRequest)."))
 ((name . "DefinitionParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "Parameters for a [DefinitionRequest](#DefinitionRequest)."))
 ((name . "DefinitionRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "DefinitionOptions"))])
  (documentation .
		 "Registration options for a [DefinitionRequest](#DefinitionRequest)."))
 ((name . "ReferenceParams")
  (properties .
	      [((name . "context")
		(type (kind . "reference") (name . "ReferenceContext")))])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "Parameters for a [ReferencesRequest](#ReferencesRequest)."))
 ((name . "ReferenceRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "ReferenceOptions"))])
  (documentation .
		 "Registration options for a [ReferencesRequest](#ReferencesRequest)."))
 ((name . "DocumentHighlightParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "Parameters for a [DocumentHighlightRequest](#DocumentHighlightRequest)."))
 ((name . "DocumentHighlight")
  (properties .
	      [((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range this highlight applies to."))
	       ((name . "kind")
		(type (kind . "reference")
		      (name . "DocumentHighlightKind"))
		(optional . t)
		(documentation .
			       "The highlight kind, default is [text](#DocumentHighlightKind.Text)."))])
  (documentation .
		 "A document highlight is a range inside a text document which deserves\nspecial attention. Usually a document highlight is visualized by changing\nthe background color of its range."))
 ((name . "DocumentHighlightRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "DocumentHighlightOptions"))])
  (documentation .
		 "Registration options for a [DocumentHighlightRequest](#DocumentHighlightRequest)."))
 ((name . "DocumentSymbolParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "Parameters for a [DocumentSymbolRequest](#DocumentSymbolRequest)."))
 ((name . "SymbolInformation")
  (properties .
	      [((name . "deprecated")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Indicates if this symbol is deprecated.\n\n@deprecated Use tags instead"))
	       ((name . "location")
		(type (kind . "reference") (name . "Location"))
		(documentation .
			       "The location of this symbol. The location's range is used by a tool\nto reveal the location in the editor. If the symbol is selected in the\ntool the range's start information is used to position the cursor. So\nthe range usually spans more than the actual symbol's name and does\nnormally include things like visibility modifiers.\n\nThe range doesn't have to denote a node range in the sense of an abstract\nsyntax tree. It can therefore not be used to re-construct a hierarchy of\nthe symbols."))])
  (extends . [((kind . "reference") (name . "BaseSymbolInformation"))])
  (documentation .
		 "Represents information about programming constructs like variables, classes,\ninterfaces etc."))
 ((name . "DocumentSymbol")
  (properties .
	      [((name . "name")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The name of this symbol. Will be displayed in the user interface and therefore must not be\nan empty string or a string only consisting of white spaces."))
	       ((name . "detail")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "More detail for this symbol, e.g the signature of a function."))
	       ((name . "kind")
		(type (kind . "reference") (name . "SymbolKind"))
		(documentation . "The kind of this symbol."))
	       ((name . "tags")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "SymbolTag")))
		(optional . t)
		(documentation .
			       "Tags for this document symbol.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "deprecated")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Indicates if this symbol is deprecated.\n\n@deprecated Use tags instead"))
	       ((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range enclosing this symbol not including leading/trailing whitespace but everything else\nlike comments. This information is typically used to determine if the clients cursor is\ninside the symbol to reveal in the symbol in the UI."))
	       ((name . "selectionRange")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range that should be selected and revealed when this symbol is being picked, e.g the name of a function.\nMust be contained by the `range`."))
	       ((name . "children")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "DocumentSymbol")))
		(optional . t)
		(documentation .
			       "Children of this symbol, e.g. properties of a class."))])
  (documentation .
		 "Represents programming constructs like variables, classes, interfaces etc.\nthat appear in a document. Document symbols can be hierarchical and they\nhave two ranges: one that encloses its definition and one that points to\nits most interesting range, e.g. the range of an identifier."))
 ((name . "DocumentSymbolRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "DocumentSymbolOptions"))])
  (documentation .
		 "Registration options for a [DocumentSymbolRequest](#DocumentSymbolRequest)."))
 ((name . "CodeActionParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation .
			       "The document in which the command was invoked."))
	       ((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range for which the command was invoked."))
	       ((name . "context")
		(type (kind . "reference")
		      (name . "CodeActionContext"))
		(documentation .
			       "Context carrying additional information."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "The parameters of a [CodeActionRequest](#CodeActionRequest)."))
 ((name . "Command")
  (properties .
	      [((name . "title")
		(type (kind . "base") (name . "string"))
		(documentation . "Title of the command, like `save`."))
	       ((name . "command")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The identifier of the actual command handler."))
	       ((name . "arguments")
		(type (kind . "array")
		      (element (kind . "reference") (name . "LSPAny")))
		(optional . t)
		(documentation .
			       "Arguments that the command handler should be\ninvoked with."))])
  (documentation .
		 "Represents a reference to a command. Provides a title which\nwill be used to represent a command in the UI and, optionally,\nan array of arguments which will be passed to the command handler\nfunction when invoked."))
 ((name . "CodeAction")
  (properties .
	      [((name . "title")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "A short, human-readable, title for this code action."))
	       ((name . "kind")
		(type (kind . "reference") (name . "CodeActionKind"))
		(optional . t)
		(documentation .
			       "The kind of the code action.\n\nUsed to filter code actions."))
	       ((name . "diagnostics")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "Diagnostic")))
		(optional . t)
		(documentation .
			       "The diagnostics that this code action resolves."))
	       ((name . "isPreferred")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Marks this as a preferred action. Preferred actions are used by the `auto fix` command and can be targeted\nby keybindings.\n\nA quick fix should be marked preferred if it properly addresses the underlying error.\nA refactoring should be marked preferred if it is the most reasonable choice of actions to take.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "disabled")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "reason")
				     (type (kind . "base")
					   (name . "string"))
				     (documentation .
						    "Human readable description of why the code action is currently disabled.\n\nThis is displayed in the code actions UI."))])))
		(optional . t)
		(documentation .
			       "Marks that the code action cannot currently be applied.\n\nClients should follow the following guidelines regarding disabled code actions:\n\n  - Disabled code actions are not shown in automatic [lightbulbs](https://code.visualstudio.com/docs/editor/editingevolved#_code-action)\n    code action menus.\n\n  - Disabled actions are shown as faded out in the code action menu when the user requests a more specific type\n    of code action, such as refactorings.\n\n  - If the user has a [keybinding](https://code.visualstudio.com/docs/editor/refactoring#_keybindings-for-code-actions)\n    that auto applies a code action and only disabled code actions are returned, the client should show the user an\n    error message with `reason` in the editor.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "edit")
		(type (kind . "reference") (name . "WorkspaceEdit"))
		(optional . t)
		(documentation .
			       "The workspace edit this code action performs."))
	       ((name . "command")
		(type (kind . "reference") (name . "Command"))
		(optional . t)
		(documentation .
			       "A command this code action executes. If a code action\nprovides an edit and a command, first the edit is\nexecuted and then the command."))
	       ((name . "data")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "A data entry field that is preserved on a code action between\na `textDocument/codeAction` and a `codeAction/resolve` request.\n\n@since 3.16.0")
		(since . "3.16.0"))])
  (documentation .
		 "A code action represents a change that can be performed in code, e.g. to fix a problem or\nto refactor code.\n\nA CodeAction must set either `edit` and/or a `command`. If both are supplied, the `edit` is applied first, then the `command` is executed."))
 ((name . "CodeActionRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "CodeActionOptions"))])
  (documentation .
		 "Registration options for a [CodeActionRequest](#CodeActionRequest)."))
 ((name . "WorkspaceSymbolParams")
  (properties .
	      [((name . "query")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "A query string to filter symbols by. Clients may send an empty\nstring here to request all symbols."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "The parameters of a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest)."))
 ((name . "WorkspaceSymbol")
  (properties .
	      [((name . "location")
		(type (kind . "or")
		      (items .
			     [((kind . "reference")
			       (name . "Location"))
			      ((kind . "literal")
			       (value
				(properties .
					    [((name . "uri")
					      (type (kind . "base")
						    (name .
							  "DocumentUri")))])))]))
		(documentation .
			       "The location of the symbol. Whether a server is allowed to\nreturn a location without a range depends on the client\ncapability `workspace.symbol.resolveSupport`.\n\nSee SymbolInformation#location for more details."))
	       ((name . "data")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "A data entry field that is preserved on a workspace symbol between a\nworkspace symbol request and a workspace symbol resolve request."))])
  (extends . [((kind . "reference") (name . "BaseSymbolInformation"))])
  (documentation .
		 "A special workspace symbol that supports locations without a range.\n\nSee also SymbolInformation.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "WorkspaceSymbolRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference") (name . "WorkspaceSymbolOptions"))])
  (documentation .
		 "Registration options for a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest)."))
 ((name . "CodeLensParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation .
			       "The document to request code lens for."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "The parameters of a [CodeLensRequest](#CodeLensRequest)."))
 ((name . "CodeLens")
  (properties .
	      [((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range in which this code lens is valid. Should only span a single line."))
	       ((name . "command")
		(type (kind . "reference") (name . "Command"))
		(optional . t)
		(documentation .
			       "The command this code lens represents."))
	       ((name . "data")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "A data entry field that is preserved on a code lens item between\na [CodeLensRequest](#CodeLensRequest) and a [CodeLensResolveRequest]\n(#CodeLensResolveRequest)"))])
  (documentation .
		 "A code lens represents a [command](#Command) that should be shown along with\nsource text, like the number of references, a way to run tests, etc.\n\nA code lens is _unresolved_ when no command is associated to it. For performance\nreasons the creation of a code lens and resolving should be done in two stages."))
 ((name . "CodeLensRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "CodeLensOptions"))])
  (documentation .
		 "Registration options for a [CodeLensRequest](#CodeLensRequest)."))
 ((name . "DocumentLinkParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation .
			       "The document to provide document links for."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressParams"))
	   ((kind . "reference") (name . "PartialResultParams"))])
  (documentation .
		 "The parameters of a [DocumentLinkRequest](#DocumentLinkRequest)."))
 ((name . "DocumentLink")
  (properties .
	      [((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation . "The range this link applies to."))
	       ((name . "target")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The uri this link points to. If missing a resolve request is sent later."))
	       ((name . "tooltip")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The tooltip text when you hover over this link.\n\nIf a tooltip is provided, is will be displayed in a string that includes instructions on how to\ntrigger the link, such as `{0} (ctrl + click)`. The specific instructions vary depending on OS,\nuser settings, and localization.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "data")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "A data entry field that is preserved on a document link between a\nDocumentLinkRequest and a DocumentLinkResolveRequest."))])
  (documentation .
		 "A document link is a range in a text document that links to an internal or external resource, like another\ntext document or a web site."))
 ((name . "DocumentLinkRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "DocumentLinkOptions"))])
  (documentation .
		 "Registration options for a [DocumentLinkRequest](#DocumentLinkRequest)."))
 ((name . "DocumentFormattingParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The document to format."))
	       ((name . "options")
		(type (kind . "reference")
		      (name . "FormattingOptions"))
		(documentation . "The format options."))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation .
		 "The parameters of a [DocumentFormattingRequest](#DocumentFormattingRequest)."))
 ((name . "DocumentFormattingRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "DocumentFormattingOptions"))])
  (documentation .
		 "Registration options for a [DocumentFormattingRequest](#DocumentFormattingRequest)."))
 ((name . "DocumentRangeFormattingParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The document to format."))
	       ((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation . "The range to format"))
	       ((name . "options")
		(type (kind . "reference")
		      (name . "FormattingOptions"))
		(documentation . "The format options"))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation .
		 "The parameters of a [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest)."))
 ((name . "DocumentRangeFormattingRegistrationOptions")
  (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference")
	     (name . "DocumentRangeFormattingOptions"))])
  (documentation .
		 "Registration options for a [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest)."))
 ((name . "DocumentOnTypeFormattingParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The document to format."))
	       ((name . "position")
		(type (kind . "reference") (name . "Position"))
		(documentation .
			       "The position around which the on type formatting should happen.\nThis is not necessarily the exact position where the character denoted\nby the property `ch` got typed."))
	       ((name . "ch") (type (kind . "base") (name . "string"))
		(documentation .
			       "The character that has been typed that triggered the formatting\non type request. That is not necessarily the last character that\ngot inserted into the document since the client could auto insert\ncharacters as well (e.g. like automatic brace completion)."))
	       ((name . "options")
		(type (kind . "reference")
		      (name . "FormattingOptions"))
		(documentation . "The formatting options."))])
  (documentation .
		 "The parameters of a [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest)."))
 ((name . "DocumentOnTypeFormattingRegistrationOptions")
  (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference")
	     (name . "DocumentOnTypeFormattingOptions"))])
  (documentation .
		 "Registration options for a [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest)."))
 ((name . "RenameParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The document to rename."))
	       ((name . "position")
		(type (kind . "reference") (name . "Position"))
		(documentation .
			       "The position at which this request was sent."))
	       ((name . "newName")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The new name of the symbol. If the given name is not valid the\nrequest must return a [ResponseError](#ResponseError) with an\nappropriate message set."))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation .
		 "The parameters of a [RenameRequest](#RenameRequest)."))
 ((name . "RenameRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentRegistrationOptions"))
	    ((kind . "reference") (name . "RenameOptions"))])
  (documentation .
		 "Registration options for a [RenameRequest](#RenameRequest)."))
 ((name . "PrepareRenameParams") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "TextDocumentPositionParams"))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))]))
 ((name . "ExecuteCommandParams")
  (properties .
	      [((name . "command")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The identifier of the actual command handler."))
	       ((name . "arguments")
		(type (kind . "array")
		      (element (kind . "reference") (name . "LSPAny")))
		(optional . t)
		(documentation .
			       "Arguments that the command should be invoked with."))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation .
		 "The parameters of a [ExecuteCommandRequest](#ExecuteCommandRequest)."))
 ((name . "ExecuteCommandRegistrationOptions") (properties . [])
  (extends . [((kind . "reference") (name . "ExecuteCommandOptions"))])
  (documentation .
		 "Registration options for a [ExecuteCommandRequest](#ExecuteCommandRequest)."))
 ((name . "ApplyWorkspaceEditParams")
  (properties .
	      [((name . "label")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "An optional label of the workspace edit. This label is\npresented in the user interface for example on an undo\nstack to undo the workspace edit."))
	       ((name . "edit")
		(type (kind . "reference") (name . "WorkspaceEdit"))
		(documentation . "The edits to apply."))])
  (documentation .
		 "The parameters passed via a apply workspace edit request."))
 ((name . "ApplyWorkspaceEditResult")
  (properties .
	      [((name . "applied")
		(type (kind . "base") (name . "boolean"))
		(documentation .
			       "Indicates whether the edit was applied or not."))
	       ((name . "failureReason")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "An optional textual description for why the edit was not applied.\nThis may be used by the server for diagnostic logging or to provide\na suitable error for a request that triggered the edit."))
	       ((name . "failedChange")
		(type (kind . "base") (name . "uinteger"))
		(optional . t)
		(documentation .
			       "Depending on the client's failure handling strategy `failedChange` might\ncontain the index of the change that failed. This property is only available\nif the client signals a `failureHandlingStrategy` in its client capabilities."))])
  (documentation .
		 "The result returned from the apply workspace edit request.\n\n@since 3.17 renamed from ApplyWorkspaceEditResponse")
  (since . "3.17 renamed from ApplyWorkspaceEditResponse"))
 ((name . "WorkDoneProgressBegin")
  (properties .
	      [((name . "kind")
		(type (kind . "stringLiteral") (value . "begin")))
	       ((name . "title")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "Mandatory title of the progress operation. Used to briefly inform about\nthe kind of operation being performed.\n\nExamples: \"Indexing\" or \"Linking dependencies\"."))
	       ((name . "cancellable")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Controls if a cancel button should show to allow the user to cancel the\nlong running operation. Clients that don't support cancellation are allowed\nto ignore the setting."))
	       ((name . "message")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "Optional, more detailed associated progress message. Contains\ncomplementary information to the `title`.\n\nExamples: \"3/25 files\", \"project/src/module2\", \"node_modules/some_dep\".\nIf unset, the previous progress message (if any) is still valid."))
	       ((name . "percentage")
		(type (kind . "base") (name . "uinteger"))
		(optional . t)
		(documentation .
			       "Optional progress percentage to display (value 100 is considered 100%).\nIf not provided infinite progress is assumed and clients are allowed\nto ignore the `percentage` value in subsequent in report notifications.\n\nThe value should be steadily rising. Clients are free to ignore values\nthat are not following this rule. The value range is [0, 100]."))]))
 ((name . "WorkDoneProgressReport")
  (properties .
	      [((name . "kind")
		(type (kind . "stringLiteral") (value . "report")))
	       ((name . "cancellable")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Controls enablement state of a cancel button.\n\nClients that don't support cancellation or don't support controlling the button's\nenablement state are allowed to ignore the property."))
	       ((name . "message")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "Optional, more detailed associated progress message. Contains\ncomplementary information to the `title`.\n\nExamples: \"3/25 files\", \"project/src/module2\", \"node_modules/some_dep\".\nIf unset, the previous progress message (if any) is still valid."))
	       ((name . "percentage")
		(type (kind . "base") (name . "uinteger"))
		(optional . t)
		(documentation .
			       "Optional progress percentage to display (value 100 is considered 100%).\nIf not provided infinite progress is assumed and clients are allowed\nto ignore the `percentage` value in subsequent in report notifications.\n\nThe value should be steadily rising. Clients are free to ignore values\nthat are not following this rule. The value range is [0, 100]"))]))
 ((name . "WorkDoneProgressEnd")
  (properties .
	      [((name . "kind")
		(type (kind . "stringLiteral") (value . "end")))
	       ((name . "message")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "Optional, a final message indicating to for example indicate the outcome\nof the operation."))]))
 ((name . "SetTraceParams")
  (properties .
	      [((name . "value")
		(type (kind . "reference") (name . "TraceValues")))]))
 ((name . "LogTraceParams")
  (properties .
	      [((name . "message")
		(type (kind . "base") (name . "string")))
	       ((name . "verbose")
		(type (kind . "base") (name . "string"))
		(optional . t))]))
 ((name . "CancelParams")
  (properties .
	      [((name . "id")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "integer"))
			      ((kind . "base") (name . "string"))]))
		(documentation . "The request id to cancel."))]))
 ((name . "ProgressParams")
  (properties .
	      [((name . "token")
		(type (kind . "reference") (name . "ProgressToken"))
		(documentation .
			       "The progress token provided by the client or server."))
	       ((name . "value")
		(type (kind . "reference") (name . "LSPAny"))
		(documentation . "The progress data."))]))
 ((name . "TextDocumentPositionParams")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentIdentifier"))
		(documentation . "The text document."))
	       ((name . "position")
		(type (kind . "reference") (name . "Position"))
		(documentation .
			       "The position inside the text document."))])
  (documentation .
		 "A parameter literal used in requests to pass a text document and a position inside that\ndocument."))
 ((name . "WorkDoneProgressParams")
  (properties .
	      [((name . "workDoneToken")
		(type (kind . "reference") (name . "ProgressToken"))
		(optional . t)
		(documentation .
			       "An optional token that a server can use to report work done progress."))]))
 ((name . "LocationLink")
  (properties .
	      [((name . "originSelectionRange")
		(type (kind . "reference") (name . "Range"))
		(optional . t)
		(documentation .
			       "Span of the origin of this link.\n\nUsed as the underlined span for mouse interaction. Defaults to the word range at\nthe definition position."))
	       ((name . "targetUri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation .
			       "The target resource identifier of this link."))
	       ((name . "targetRange")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The full target range of this link. If the target for example is a symbol then target range is the\nrange enclosing this symbol not including leading/trailing whitespace but everything else\nlike comments. This information is typically used to highlight the range in the editor."))
	       ((name . "targetSelectionRange")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range that should be selected and revealed when this link is being followed, e.g the name of a function.\nMust be contained by the `targetRange`. See also `DocumentSymbol#range`"))])
  (documentation .
		 "Represents the connection of two locations. Provides additional metadata over normal [locations](#Location),\nincluding an origin range."))
 ((name . "Range")
  (properties .
	      [((name . "start")
		(type (kind . "reference") (name . "Position"))
		(documentation . "The range's start position."))
	       ((name . "end")
		(type (kind . "reference") (name . "Position"))
		(documentation . "The range's end position."))])
  (documentation .
		 "A range in a text document expressed as (zero-based) start and end positions.\n\nIf you want to specify a range that contains a line including the line ending\ncharacter(s) then use an end position denoting the start of the next line.\nFor example:\n```ts\n{\n    start: { line: 5, character: 23 }\n    end : { line 6, character : 0 }\n}\n```"))
 ((name . "ImplementationOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))]))
 ((name . "StaticRegistrationOptions")
  (properties .
	      [((name . "id") (type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The id used to register the request. The id can be used to deregister\nthe request again. See also Registration#id."))])
  (documentation .
		 "Static registration options to be returned in the initialize\nrequest."))
 ((name . "TypeDefinitionOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))]))
 ((name . "WorkspaceFoldersChangeEvent")
  (properties .
	      [((name . "added")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "WorkspaceFolder")))
		(documentation .
			       "The array of added workspace folders"))
	       ((name . "removed")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "WorkspaceFolder")))
		(documentation .
			       "The array of the removed workspace folders"))])
  (documentation . "The workspace folder change event."))
 ((name . "ConfigurationItem")
  (properties .
	      [((name . "scopeUri")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The scope to get the configuration section for."))
	       ((name . "section")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The configuration section asked for."))]))
 ((name . "TextDocumentIdentifier")
  (properties .
	      [((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation . "The text document's uri."))])
  (documentation .
		 "A literal to identify a text document in the client."))
 ((name . "Color")
  (properties .
	      [((name . "red")
		(type (kind . "base") (name . "decimal"))
		(documentation .
			       "The red component of this color in the range [0-1]."))
	       ((name . "green")
		(type (kind . "base") (name . "decimal"))
		(documentation .
			       "The green component of this color in the range [0-1]."))
	       ((name . "blue")
		(type (kind . "base") (name . "decimal"))
		(documentation .
			       "The blue component of this color in the range [0-1]."))
	       ((name . "alpha")
		(type (kind . "base") (name . "decimal"))
		(documentation .
			       "The alpha component of this color in the range [0-1]."))])
  (documentation . "Represents a color in RGBA space."))
 ((name . "DocumentColorOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))]))
 ((name . "FoldingRangeOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))]))
 ((name . "DeclarationOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))]))
 ((name . "Position")
  (properties .
	      [((name . "line")
		(type (kind . "base") (name . "uinteger"))
		(documentation .
			       "Line position in a document (zero-based).\n\nIf a line number is greater than the number of lines in a document, it defaults back to the number of lines in the document.\nIf a line number is negative, it defaults to 0."))
	       ((name . "character")
		(type (kind . "base") (name . "uinteger"))
		(documentation .
			       "Character offset on a line in a document (zero-based).\n\nThe meaning of this offset is determined by the negotiated\n`PositionEncodingKind`.\n\nIf the character value is greater than the line length it defaults back to the\nline length."))])
  (documentation .
		 "Position in a text document expressed as zero-based line and character\noffset. Prior to 3.17 the offsets were always based on a UTF-16 string\nrepresentation. So a string of the form `a𐐀b` the character offset of the\ncharacter `a` is 0, the character offset of `𐐀` is 1 and the character\noffset of b is 3 since `𐐀` is represented using two code units in UTF-16.\nSince 3.17 clients and servers can agree on a different string encoding\nrepresentation (e.g. UTF-8). The client announces it's supported encoding\nvia the client capability [`general.positionEncodings`](#clientCapabilities).\nThe value is an array of position encodings the client supports, with\ndecreasing preference (e.g. the encoding at index `0` is the most preferred\none). To stay backwards compatible the only mandatory encoding is UTF-16\nrepresented via the string `utf-16`. The server can pick one of the\nencodings offered by the client and signals that encoding back to the\nclient via the initialize result's property\n[`capabilities.positionEncoding`](#serverCapabilities). If the string value\n`utf-16` is missing from the client's capability `general.positionEncodings`\nservers can safely assume that the client supports UTF-16. If the server\nomits the position encoding in its initialize result the encoding defaults\nto the string value `utf-16`. Implementation considerations: since the\nconversion from one encoding into another requires the content of the\nfile / line the conversion is best done where the file is read which is\nusually on the server side.\n\nPositions are line end character agnostic. So you can not specify a position\nthat denotes `\\r|\\n` or `\\n|` where `|` represents the character offset.\n\n@since 3.17.0 - support for negotiated position encoding.")
  (since . "3.17.0 - support for negotiated position encoding."))
 ((name . "SelectionRangeOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))]))
 ((name . "CallHierarchyOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Call hierarchy options used during static registration.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "SemanticTokensOptions")
  (properties .
	      [((name . "legend")
		(type (kind . "reference")
		      (name . "SemanticTokensLegend"))
		(documentation . "The legend used by the server"))
	       ((name . "range")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "literal")
			       (value (properties . [])))]))
		(optional . t)
		(documentation .
			       "Server supports providing semantic tokens for a specific range\nof a document."))
	       ((name . "full")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "literal")
			       (value
				(properties .
					    [((name . "delta")
					      (type (kind . "base")
						    (name . "boolean"))
					      (optional . t)
					      (documentation .
							     "The server supports deltas for full documents."))])))]))
		(optional . t)
		(documentation .
			       "Server supports providing semantic tokens for a full document."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "SemanticTokensEdit")
  (properties .
	      [((name . "start")
		(type (kind . "base") (name . "uinteger"))
		(documentation . "The start offset of the edit."))
	       ((name . "deleteCount")
		(type (kind . "base") (name . "uinteger"))
		(documentation . "The count of elements to remove."))
	       ((name . "data")
		(type (kind . "array")
		      (element (kind . "base") (name . "uinteger")))
		(optional . t)
		(documentation . "The elements to insert."))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "LinkedEditingRangeOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))]))
 ((name . "FileCreate")
  (properties .
	      [((name . "uri")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "A file:// URI for the location of the file/folder being created."))])
  (documentation .
		 "Represents information on a file/folder create.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "TextDocumentEdit")
  (properties .
	      [((name . "textDocument")
		(type (kind . "reference")
		      (name .
			    "OptionalVersionedTextDocumentIdentifier"))
		(documentation . "The text document to change."))
	       ((name . "edits")
		(type (kind . "array")
		      (element (kind . "or")
			       (items .
				      [((kind . "reference")
					(name . "TextEdit"))
				       ((kind . "reference")
					(name . "AnnotatedTextEdit"))])))
		(documentation .
			       "The edits to be applied.\n\n@since 3.16.0 - support for AnnotatedTextEdit. This is guarded using a\nclient capability.")
		(since .
		       "3.16.0 - support for AnnotatedTextEdit. This is guarded using a\nclient capability."))])
  (documentation .
		 "Describes textual changes on a text document. A TextDocumentEdit describes all changes\non a document version Si and after they are applied move the document to version Si+1.\nSo the creator of a TextDocumentEdit doesn't need to sort the array of edits or do any\nkind of ordering. However the edits must be non overlapping."))
 ((name . "CreateFile")
  (properties .
	      [((name . "kind")
		(type (kind . "stringLiteral") (value . "create"))
		(documentation . "A create"))
	       ((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation . "The resource to create."))
	       ((name . "options")
		(type (kind . "reference")
		      (name . "CreateFileOptions"))
		(optional . t) (documentation . "Additional options"))])
  (extends . [((kind . "reference") (name . "ResourceOperation"))])
  (documentation . "Create file operation."))
 ((name . "RenameFile")
  (properties .
	      [((name . "kind")
		(type (kind . "stringLiteral") (value . "rename"))
		(documentation . "A rename"))
	       ((name . "oldUri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation . "The old (existing) location."))
	       ((name . "newUri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation . "The new location."))
	       ((name . "options")
		(type (kind . "reference")
		      (name . "RenameFileOptions"))
		(optional . t) (documentation . "Rename options."))])
  (extends . [((kind . "reference") (name . "ResourceOperation"))])
  (documentation . "Rename file operation"))
 ((name . "DeleteFile")
  (properties .
	      [((name . "kind")
		(type (kind . "stringLiteral") (value . "delete"))
		(documentation . "A delete"))
	       ((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation . "The file to delete."))
	       ((name . "options")
		(type (kind . "reference")
		      (name . "DeleteFileOptions"))
		(optional . t) (documentation . "Delete options."))])
  (extends . [((kind . "reference") (name . "ResourceOperation"))])
  (documentation . "Delete file operation"))
 ((name . "ChangeAnnotation")
  (properties .
	      [((name . "label")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "A human-readable string describing the actual change. The string\nis rendered prominent in the user interface."))
	       ((name . "needsConfirmation")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "A flag which indicates that user confirmation is needed\nbefore applying the change."))
	       ((name . "description")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "A human-readable string which is rendered less prominent in\nthe user interface."))])
  (documentation .
		 "Additional information that describes document changes.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "FileOperationFilter")
  (properties .
	      [((name . "scheme")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "A Uri scheme like `file` or `untitled`."))
	       ((name . "pattern")
		(type (kind . "reference")
		      (name . "FileOperationPattern"))
		(documentation . "The actual file operation pattern."))])
  (documentation .
		 "A filter to describe in which file operation requests or notifications\nthe server is interested in receiving.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "FileRename")
  (properties .
	      [((name . "oldUri")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "A file:// URI for the original location of the file/folder being renamed."))
	       ((name . "newUri")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "A file:// URI for the new location of the file/folder being renamed."))])
  (documentation .
		 "Represents information on a file/folder rename.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "FileDelete")
  (properties .
	      [((name . "uri")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "A file:// URI for the location of the file/folder being deleted."))])
  (documentation .
		 "Represents information on a file/folder delete.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "MonikerOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))]))
 ((name . "TypeHierarchyOptions")
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (properties . [])
  (documentation .
		 "Type hierarchy options used during static registration.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlineValueContext")
  (properties .
	      [((name . "frameId")
		(type (kind . "base") (name . "integer"))
		(documentation .
			       "The stack frame (as a DAP Id) where the execution has stopped."))
	       ((name . "stoppedLocation")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The document range where execution has stopped.\nTypically the end position of the range denotes the line where the inline values are shown."))])
  (documentation . "@since 3.17.0") (since . "3.17.0"))
 ((name . "InlineValueText")
  (properties .
	      [((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The document range for which the inline value applies."))
	       ((name . "text")
		(type (kind . "base") (name . "string"))
		(documentation . "The text of the inline value."))])
  (documentation . "Provide inline value as text.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlineValueVariableLookup")
  (properties .
	      [((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The document range for which the inline value applies.\nThe range is used to extract the variable name from the underlying document."))
	       ((name . "variableName")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "If specified the name of the variable to look up."))
	       ((name . "caseSensitiveLookup")
		(type (kind . "base") (name . "boolean"))
		(documentation . "How to perform the lookup."))])
  (documentation .
		 "Provide inline value through a variable lookup.\nIf only a range is specified, the variable name will be extracted from the underlying document.\nAn optional variable name can be used to override the extracted name.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlineValueEvaluatableExpression")
  (properties .
	      [((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The document range for which the inline value applies.\nThe range is used to extract the evaluatable expression from the underlying document."))
	       ((name . "expression")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "If specified the expression overrides the extracted expression."))])
  (documentation .
		 "Provide an inline value through an expression evaluation.\nIf only a range is specified, the expression will be extracted from the underlying document.\nAn optional expression can be used to override the extracted expression.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlineValueOptions")
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (properties . [])
  (documentation .
		 "Inline value options used during static registration.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlayHintLabelPart")
  (properties .
	      [((name . "value")
		(type (kind . "base") (name . "string"))
		(documentation . "The value of this label part."))
	       ((name . "tooltip")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "reference")
			       (name . "MarkupContent"))]))
		(optional . t)
		(documentation .
			       "The tooltip text when you hover over this label part. Depending on\nthe client capability `inlayHint.resolveSupport` clients might resolve\nthis property late using the resolve request."))
	       ((name . "location")
		(type (kind . "reference") (name . "Location"))
		(optional . t)
		(documentation .
			       "An optional source code location that represents this\nlabel part.\n\nThe editor will use this location for the hover and for code navigation\nfeatures: This part will become a clickable link that resolves to the\ndefinition of the symbol at the given location (not necessarily the\nlocation itself), it shows the hover that shows at the given location,\nand it shows a context menu with further code navigation commands.\n\nDepending on the client capability `inlayHint.resolveSupport` clients\nmight resolve this property late using the resolve request."))
	       ((name . "command")
		(type (kind . "reference") (name . "Command"))
		(optional . t)
		(documentation .
			       "An optional command for this label part.\n\nDepending on the client capability `inlayHint.resolveSupport` clients\nmight resolve this property late using the resolve request."))])
  (documentation .
		 "An inlay hint label part allows for interactive and composite labels\nof inlay hints.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "MarkupContent")
  (properties .
	      [((name . "kind")
		(type (kind . "reference") (name . "MarkupKind"))
		(documentation . "The type of the Markup"))
	       ((name . "value")
		(type (kind . "base") (name . "string"))
		(documentation . "The content itself"))])
  (documentation .
		 "A `MarkupContent` literal represents a string value which content is interpreted base on its\nkind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.\n\nIf the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.\nSee https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting\n\nHere is an example how such a string can be constructed using JavaScript / TypeScript:\n```ts\nlet markdown: MarkdownContent = {\n kind: MarkupKind.Markdown,\n value: [\n   '# Header',\n   'Some text',\n   '```typescript',\n   'someCode();',\n   '```'\n ].join('\\n')\n};\n```\n\n*Please Note* that clients might sanitize the return markdown. A client could decide to\nremove HTML from the markdown to avoid script execution."))
 ((name . "InlayHintOptions")
  (properties .
	      [((name . "resolveProvider")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The server provides support to resolve additional\ninformation for an inlay hint item."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Inlay hint options used during static registration.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "RelatedFullDocumentDiagnosticReport")
  (properties .
	      [((name . "relatedDocuments")
		(type (kind . "map")
		      (key (kind . "base") (name . "DocumentUri"))
		      (value (kind . "or")
			     (items .
				    [((kind . "reference")
				      (name .
					    "FullDocumentDiagnosticReport"))
				     ((kind . "reference")
				      (name .
					    "UnchangedDocumentDiagnosticReport"))])))
		(optional . t)
		(documentation .
			       "Diagnostics of related documents. This information is useful\nin programming languages where code in a file A can generate\ndiagnostics in a file B which A depends on. An example of\nsuch a language is C/C++ where marco definitions in a file\na.cpp and result in errors in a header file b.hpp.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (extends .
	   [((kind . "reference")
	     (name . "FullDocumentDiagnosticReport"))])
  (documentation .
		 "A full diagnostic report with a set of related documents.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "RelatedUnchangedDocumentDiagnosticReport")
  (properties .
	      [((name . "relatedDocuments")
		(type (kind . "map")
		      (key (kind . "base") (name . "DocumentUri"))
		      (value (kind . "or")
			     (items .
				    [((kind . "reference")
				      (name .
					    "FullDocumentDiagnosticReport"))
				     ((kind . "reference")
				      (name .
					    "UnchangedDocumentDiagnosticReport"))])))
		(optional . t)
		(documentation .
			       "Diagnostics of related documents. This information is useful\nin programming languages where code in a file A can generate\ndiagnostics in a file B which A depends on. An example of\nsuch a language is C/C++ where marco definitions in a file\na.cpp and result in errors in a header file b.hpp.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (extends .
	   [((kind . "reference")
	     (name . "UnchangedDocumentDiagnosticReport"))])
  (documentation .
		 "An unchanged diagnostic report with a set of related documents.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "FullDocumentDiagnosticReport")
  (properties .
	      [((name . "kind")
		(type (kind . "stringLiteral") (value . "full"))
		(documentation . "A full document diagnostic report."))
	       ((name . "resultId")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "An optional result id. If provided it will\nbe sent on the next diagnostic request for the\nsame document."))
	       ((name . "items")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "Diagnostic")))
		(documentation . "The actual items."))])
  (documentation .
		 "A diagnostic report with a full set of problems.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "UnchangedDocumentDiagnosticReport")
  (properties .
	      [((name . "kind")
		(type (kind . "stringLiteral") (value . "unchanged"))
		(documentation .
			       "A document diagnostic report indicating\nno changes to the last result. A server can\nonly return `unchanged` if result ids are\nprovided."))
	       ((name . "resultId")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "A result id which will be sent on the next\ndiagnostic request for the same document."))])
  (documentation .
		 "A diagnostic report indicating that the last returned\nreport is still accurate.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DiagnosticOptions")
  (properties .
	      [((name . "identifier")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "An optional identifier under which the diagnostics are\nmanaged by the client."))
	       ((name . "interFileDependencies")
		(type (kind . "base") (name . "boolean"))
		(documentation .
			       "Whether the language has inter file dependencies meaning that\nediting code in one file can result in a different diagnostic\nset in another file. Inter file dependencies are common for\nmost programming languages and typically uncommon for linters."))
	       ((name . "workspaceDiagnostics")
		(type (kind . "base") (name . "boolean"))
		(documentation .
			       "The server provides support for workspace diagnostics as well."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation . "Diagnostic options.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "PreviousResultId")
  (properties .
	      [((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation .
			       "The URI for which the client knowns a\nresult id."))
	       ((name . "value")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The value of the previous result id."))])
  (documentation .
		 "A previous result id in a workspace pull request.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "NotebookDocument")
  (properties .
	      [((name . "uri") (type (kind . "base") (name . "URI"))
		(documentation . "The notebook document's uri."))
	       ((name . "notebookType")
		(type (kind . "base") (name . "string"))
		(documentation . "The type of the notebook."))
	       ((name . "version")
		(type (kind . "base") (name . "integer"))
		(documentation .
			       "The version number of this document (it will increase after each\nchange, including undo/redo)."))
	       ((name . "metadata")
		(type (kind . "reference") (name . "LSPObject"))
		(optional . t)
		(documentation .
			       "Additional metadata stored with the notebook\ndocument.\n\nNote: should always be an object literal (e.g. LSPObject)"))
	       ((name . "cells")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "NotebookCell")))
		(documentation . "The cells of a notebook."))])
  (documentation . "A notebook document.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "TextDocumentItem")
  (properties .
	      [((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation . "The text document's uri."))
	       ((name . "languageId")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The text document's language identifier."))
	       ((name . "version")
		(type (kind . "base") (name . "integer"))
		(documentation .
			       "The version number of this document (it will increase after each\nchange, including undo/redo)."))
	       ((name . "text")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The content of the opened text document."))])
  (documentation .
		 "An item to transfer a text document from the client to the\nserver."))
 ((name . "VersionedNotebookDocumentIdentifier")
  (properties .
	      [((name . "version")
		(type (kind . "base") (name . "integer"))
		(documentation .
			       "The version number of this notebook document."))
	       ((name . "uri") (type (kind . "base") (name . "URI"))
		(documentation . "The notebook document's uri."))])
  (documentation .
		 "A versioned notebook document identifier.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "NotebookDocumentChangeEvent")
  (properties .
	      [((name . "metadata")
		(type (kind . "reference") (name . "LSPObject"))
		(optional . t)
		(documentation .
			       "The changed meta data if any.\n\nNote: should always be an object literal (e.g. LSPObject)"))
	       ((name . "cells")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "structure")
				     (type (kind . "literal")
					   (value
					    (properties .
							[((name .
								"array")
							  (type
							   (kind .
								 "reference")
							   (name .
								 "NotebookCellArrayChange"))
							  (documentation
							   .
							   "The change to the cell array."))
							 ((name .
								"didOpen")
							  (type
							   (kind .
								 "array")
							   (element
							    (kind .
								  "reference")
							    (name .
								  "TextDocumentItem")))
							  (optional .
								    t)
							  (documentation
							   .
							   "Additional opened cell text documents."))
							 ((name .
								"didClose")
							  (type
							   (kind .
								 "array")
							   (element
							    (kind .
								  "reference")
							    (name .
								  "TextDocumentIdentifier")))
							  (optional .
								    t)
							  (documentation
							   .
							   "Additional closed cell text documents."))])))
				     (optional . t)
				     (documentation .
						    "Changes to the cell structure to add or\nremove cells."))
				    ((name . "data")
				     (type (kind . "array")
					   (element
					    (kind . "reference")
					    (name . "NotebookCell")))
				     (optional . t)
				     (documentation .
						    "Changes to notebook cells properties like its\nkind, execution summary or metadata."))
				    ((name . "textContent")
				     (type (kind . "array")
					   (element (kind . "literal")
						    (value
						     (properties .
								 [((name
								    .
								    "document")
								   (type
								    (kind
								     .
								     "reference")
								    (name
								     .
								     "VersionedTextDocumentIdentifier")))
								  ((name
								    .
								    "changes")
								   (type
								    (kind
								     .
								     "array")
								    (element
								     (kind
								      .
								      "reference")
								     (name
								      .
								      "TextDocumentContentChangeEvent"))))]))))
				     (optional . t)
				     (documentation .
						    "Changes to the text content of notebook cells."))])))
		(optional . t) (documentation . "Changes to cells"))])
  (documentation .
		 "A change event for a notebook document.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "NotebookDocumentIdentifier")
  (properties .
	      [((name . "uri") (type (kind . "base") (name . "URI"))
		(documentation . "The notebook document's uri."))])
  (documentation .
		 "A literal to identify a notebook document in the client.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "Registration")
  (properties .
	      [((name . "id") (type (kind . "base") (name . "string"))
		(documentation .
			       "The id used to register the request. The id can be used to deregister\nthe request again."))
	       ((name . "method")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The method / capability to register for."))
	       ((name . "registerOptions")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "Options necessary for the registration."))])
  (documentation .
		 "General parameters to to register for an notification or to register a provider."))
 ((name . "Unregistration")
  (properties .
	      [((name . "id") (type (kind . "base") (name . "string"))
		(documentation .
			       "The id used to unregister the request or notification. Usually an id\nprovided during the register request."))
	       ((name . "method")
		(type (kind . "base") (name . "string"))
		(documentation . "The method to unregister for."))])
  (documentation .
		 "General parameters to unregister a request or notification."))
 ((name . "_InitializeParams")
  (properties .
	      [((name . "processId")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "integer"))
			      ((kind . "base") (name . "null"))]))
		(documentation .
			       "The process Id of the parent process that started\nthe server.\n\nIs `null` if the process has not been started by another process.\nIf the parent process is not alive then the server should exit."))
	       ((name . "clientInfo")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "name")
				     (type (kind . "base")
					   (name . "string"))
				     (documentation .
						    "The name of the client as defined by the client."))
				    ((name . "version")
				     (type (kind . "base")
					   (name . "string"))
				     (optional . t)
				     (documentation .
						    "The client's version as defined by the client."))])))
		(optional . t)
		(documentation .
			       "Information about the client\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "locale")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The locale the client is currently showing the user interface\nin. This must not necessarily be the locale of the operating\nsystem.\n\nUses IETF language tags as the value's syntax\n(See https://en.wikipedia.org/wiki/IETF_language_tag)\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "rootPath")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "base") (name . "null"))]))
		(optional . t)
		(documentation .
			       "The rootPath of the workspace. Is null\nif no folder is open.\n\n@deprecated in favour of rootUri."))
	       ((name . "rootUri")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "DocumentUri"))
			      ((kind . "base") (name . "null"))]))
		(documentation .
			       "The rootUri of the workspace. Is null if no\nfolder is open. If both `rootPath` and `rootUri` are set\n`rootUri` wins.\n\n@deprecated in favour of workspaceFolders."))
	       ((name . "capabilities")
		(type (kind . "reference")
		      (name . "ClientCapabilities"))
		(documentation .
			       "The capabilities provided by the client (editor or tool)"))
	       ((name . "initializationOptions")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "User provided initialization options."))
	       ((name . "trace")
		(type (kind . "or")
		      (items .
			     [((kind . "stringLiteral")
			       (value . "off"))
			      ((kind . "stringLiteral")
			       (value . "messages"))
			      ((kind . "stringLiteral")
			       (value . "compact"))
			      ((kind . "stringLiteral")
			       (value . "verbose"))]))
		(optional . t)
		(documentation .
			       "The initial trace setting. If omitted trace is disabled ('off')."))])
  (mixins . [((kind . "reference") (name . "WorkDoneProgressParams"))])
  (documentation . "The initialize parameters"))
 ((name . "WorkspaceFoldersInitializeParams")
  (properties .
	      [((name . "workspaceFolders")
		(type (kind . "or")
		      (items .
			     [((kind . "array")
			       (element (kind . "reference")
					(name . "WorkspaceFolder")))
			      ((kind . "base") (name . "null"))]))
		(optional . t)
		(documentation .
			       "The workspace folders configured in the client when the server starts.\n\nThis property is only available if the client supports workspace folders.\nIt can be `null` if the client supports workspace folders but none are\nconfigured.\n\n@since 3.6.0")
		(since . "3.6.0"))]))
 ((name . "ServerCapabilities")
  (properties .
	      [((name . "positionEncoding")
		(type (kind . "reference")
		      (name . "PositionEncodingKind"))
		(optional . t)
		(documentation .
			       "The position encoding the server picked from the encodings offered\nby the client via the client capability `general.positionEncodings`.\n\nIf the client didn't provide any position encodings the only valid\nvalue that a server can return is 'utf-16'.\n\nIf omitted it defaults to 'utf-16'.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "textDocumentSync")
		(type (kind . "or")
		      (items .
			     [((kind . "reference")
			       (name . "TextDocumentSyncOptions"))
			      ((kind . "reference")
			       (name . "TextDocumentSyncKind"))]))
		(optional . t)
		(documentation .
			       "Defines how text documents are synced. Is either a detailed structure\ndefining each notification or for backwards compatibility the\nTextDocumentSyncKind number."))
	       ((name . "notebookDocumentSync")
		(type (kind . "or")
		      (items .
			     [((kind . "reference")
			       (name . "NotebookDocumentSyncOptions"))
			      ((kind . "reference")
			       (name .
				     "NotebookDocumentSyncRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "Defines how notebook documents are synced.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "completionProvider")
		(type (kind . "reference")
		      (name . "CompletionOptions"))
		(optional . t)
		(documentation .
			       "The server provides completion support."))
	       ((name . "hoverProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "HoverOptions"))]))
		(optional . t)
		(documentation . "The server provides hover support."))
	       ((name . "signatureHelpProvider")
		(type (kind . "reference")
		      (name . "SignatureHelpOptions"))
		(optional . t)
		(documentation .
			       "The server provides signature help support."))
	       ((name . "declarationProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "DeclarationOptions"))
			      ((kind . "reference")
			       (name .
				     "DeclarationRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides Goto Declaration support."))
	       ((name . "definitionProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "DefinitionOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides goto definition support."))
	       ((name . "typeDefinitionProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "TypeDefinitionOptions"))
			      ((kind . "reference")
			       (name .
				     "TypeDefinitionRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides Goto Type Definition support."))
	       ((name . "implementationProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "ImplementationOptions"))
			      ((kind . "reference")
			       (name .
				     "ImplementationRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides Goto Implementation support."))
	       ((name . "referencesProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "ReferenceOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides find references support."))
	       ((name . "documentHighlightProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "DocumentHighlightOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides document highlight support."))
	       ((name . "documentSymbolProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "DocumentSymbolOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides document symbol support."))
	       ((name . "codeActionProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "CodeActionOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides code actions. CodeActionOptions may only be\nspecified if the client states that it supports\n`codeActionLiteralSupport` in its initial `initialize` request."))
	       ((name . "codeLensProvider")
		(type (kind . "reference") (name . "CodeLensOptions"))
		(optional . t)
		(documentation . "The server provides code lens."))
	       ((name . "documentLinkProvider")
		(type (kind . "reference")
		      (name . "DocumentLinkOptions"))
		(optional . t)
		(documentation .
			       "The server provides document link support."))
	       ((name . "colorProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "DocumentColorOptions"))
			      ((kind . "reference")
			       (name .
				     "DocumentColorRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides color provider support."))
	       ((name . "workspaceSymbolProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "WorkspaceSymbolOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides workspace symbol support."))
	       ((name . "documentFormattingProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "DocumentFormattingOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides document formatting."))
	       ((name . "documentRangeFormattingProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name .
				     "DocumentRangeFormattingOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides document range formatting."))
	       ((name . "documentOnTypeFormattingProvider")
		(type (kind . "reference")
		      (name . "DocumentOnTypeFormattingOptions"))
		(optional . t)
		(documentation .
			       "The server provides document formatting on typing."))
	       ((name . "renameProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "RenameOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides rename support. RenameOptions may only be\nspecified if the client states that it supports\n`prepareSupport` in its initial `initialize` request."))
	       ((name . "foldingRangeProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "FoldingRangeOptions"))
			      ((kind . "reference")
			       (name .
				     "FoldingRangeRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides folding provider support."))
	       ((name . "selectionRangeProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "SelectionRangeOptions"))
			      ((kind . "reference")
			       (name .
				     "SelectionRangeRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides selection range support."))
	       ((name . "executeCommandProvider")
		(type (kind . "reference")
		      (name . "ExecuteCommandOptions"))
		(optional . t)
		(documentation .
			       "The server provides execute command support."))
	       ((name . "callHierarchyProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "CallHierarchyOptions"))
			      ((kind . "reference")
			       (name .
				     "CallHierarchyRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides call hierarchy support.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "linkedEditingRangeProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "LinkedEditingRangeOptions"))
			      ((kind . "reference")
			       (name .
				     "LinkedEditingRangeRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides linked editing range support.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "semanticTokensProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "reference")
			       (name . "SemanticTokensOptions"))
			      ((kind . "reference")
			       (name .
				     "SemanticTokensRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides semantic tokens support.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "monikerProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "MonikerOptions"))
			      ((kind . "reference")
			       (name . "MonikerRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides moniker support.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "typeHierarchyProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "TypeHierarchyOptions"))
			      ((kind . "reference")
			       (name .
				     "TypeHierarchyRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides type hierarchy support.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "inlineValueProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "InlineValueOptions"))
			      ((kind . "reference")
			       (name .
				     "InlineValueRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides inline values.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "inlayHintProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "InlayHintOptions"))
			      ((kind . "reference")
			       (name . "InlayHintRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server provides inlay hints.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "diagnosticProvider")
		(type (kind . "or")
		      (items .
			     [((kind . "reference")
			       (name . "DiagnosticOptions"))
			      ((kind . "reference")
			       (name . "DiagnosticRegistrationOptions"))]))
		(optional . t)
		(documentation .
			       "The server has support for pull model diagnostics.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "workspace")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "workspaceFolders")
				     (type (kind . "reference")
					   (name .
						 "WorkspaceFoldersServerCapabilities"))
				     (optional . t)
				     (documentation .
						    "The server supports workspace folder.\n\n@since 3.6.0")
				     (since . "3.6.0"))
				    ((name . "fileOperations")
				     (type (kind . "reference")
					   (name .
						 "FileOperationOptions"))
				     (optional . t)
				     (documentation .
						    "The server is interested in notifications/requests for operations on files.\n\n@since 3.16.0")
				     (since . "3.16.0"))])))
		(optional . t)
		(documentation .
			       "Workspace specific server capabilities."))
	       ((name . "experimental")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation . "Experimental server capabilities."))])
  (documentation .
		 "Defines the capabilities provided by a language\nserver."))
 ((name . "VersionedTextDocumentIdentifier")
  (properties .
	      [((name . "version")
		(type (kind . "base") (name . "integer"))
		(documentation .
			       "The version number of this document."))])
  (extends .
	   [((kind . "reference") (name . "TextDocumentIdentifier"))])
  (documentation .
		 "A text document identifier to denote a specific version of a text document."))
 ((name . "SaveOptions")
  (properties .
	      [((name . "includeText")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client is supposed to include the content on save."))])
  (documentation . "Save options."))
 ((name . "FileEvent")
  (properties .
	      [((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation . "The file's uri."))
	       ((name . "type")
		(type (kind . "reference") (name . "FileChangeType"))
		(documentation . "The change type."))])
  (documentation . "An event describing a file change."))
 ((name . "FileSystemWatcher")
  (properties .
	      [((name . "globPattern")
		(type (kind . "reference") (name . "GlobPattern"))
		(documentation .
			       "The glob pattern to watch. See {@link GlobPattern glob pattern} for more detail.\n\n@since 3.17.0 support for relative patterns.")
		(since . "3.17.0 support for relative patterns."))
	       ((name . "kind")
		(type (kind . "reference") (name . "WatchKind"))
		(optional . t)
		(documentation .
			       "The kind of events of interest. If omitted it defaults\nto WatchKind.Create | WatchKind.Change | WatchKind.Delete\nwhich is 7."))]))
 ((name . "Diagnostic")
  (properties .
	      [((name . "range")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range at which the message applies"))
	       ((name . "severity")
		(type (kind . "reference")
		      (name . "DiagnosticSeverity"))
		(optional . t)
		(documentation .
			       "The diagnostic's severity. Can be omitted. If omitted it is up to the\nclient to interpret diagnostics as error, warning, info or hint."))
	       ((name . "code")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "integer"))
			      ((kind . "base") (name . "string"))]))
		(optional . t)
		(documentation .
			       "The diagnostic's code, which usually appear in the user interface."))
	       ((name . "codeDescription")
		(type (kind . "reference") (name . "CodeDescription"))
		(optional . t)
		(documentation .
			       "An optional property to describe the error code.\nRequires the code field (above) to be present/not null.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "source")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "A human-readable string describing the source of this\ndiagnostic, e.g. 'typescript' or 'super lint'. It usually\nappears in the user interface."))
	       ((name . "message")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The diagnostic's message. It usually appears in the user interface"))
	       ((name . "tags")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "DiagnosticTag")))
		(optional . t)
		(documentation .
			       "Additional metadata about the diagnostic.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "relatedInformation")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "DiagnosticRelatedInformation")))
		(optional . t)
		(documentation .
			       "An array of related diagnostic information, e.g. when symbol-names within\na scope collide all definitions can be marked via this property."))
	       ((name . "data")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation .
			       "A data entry field that is preserved between a `textDocument/publishDiagnostics`\nnotification and `textDocument/codeAction` request.\n\n@since 3.16.0")
		(since . "3.16.0"))])
  (documentation .
		 "Represents a diagnostic, such as a compiler error or warning. Diagnostic objects\nare only valid in the scope of a resource."))
 ((name . "CompletionContext")
  (properties .
	      [((name . "triggerKind")
		(type (kind . "reference")
		      (name . "CompletionTriggerKind"))
		(documentation . "How the completion was triggered."))
	       ((name . "triggerCharacter")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The trigger character (a single character) that has trigger code complete.\nIs undefined if `triggerKind !== CompletionTriggerKind.TriggerCharacter`"))])
  (documentation .
		 "Contains additional information about the context in which a completion request is triggered."))
 ((name . "CompletionItemLabelDetails")
  (properties .
	      [((name . "detail")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "An optional string which is rendered less prominently directly after {@link CompletionItem.label label},\nwithout any spacing. Should be used for function signatures and type annotations."))
	       ((name . "description")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "An optional string which is rendered less prominently after {@link CompletionItem.detail}. Should be used\nfor fully qualified names and file paths."))])
  (documentation .
		 "Additional details for a completion item label.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InsertReplaceEdit")
  (properties .
	      [((name . "newText")
		(type (kind . "base") (name . "string"))
		(documentation . "The string to be inserted."))
	       ((name . "insert")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range if the insert is requested"))
	       ((name . "replace")
		(type (kind . "reference") (name . "Range"))
		(documentation .
			       "The range if the replace is requested."))])
  (documentation .
		 "A special text edit to provide an insert and a replace operation.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "CompletionOptions")
  (properties .
	      [((name . "triggerCharacters")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(optional . t)
		(documentation .
			       "Most tools trigger completion request automatically without explicitly requesting\nit using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user\nstarts to type an identifier. For example if the user types `c` in a JavaScript file\ncode complete will automatically pop up present `console` besides others as a\ncompletion item. Characters that make up identifiers don't need to be listed here.\n\nIf code complete should automatically be trigger on characters not being valid inside\nan identifier (for example `.` in JavaScript) list them in `triggerCharacters`."))
	       ((name . "allCommitCharacters")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(optional . t)
		(documentation .
			       "The list of all possible characters that commit a completion. This field can be used\nif clients don't support individual commit characters per completion item. See\n`ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`\n\nIf a server provides both `allCommitCharacters` and commit characters on an individual\ncompletion item the ones on the completion item win.\n\n@since 3.2.0")
		(since . "3.2.0"))
	       ((name . "resolveProvider")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The server provides support to resolve additional\ninformation for a completion item."))
	       ((name . "completionItem")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "labelDetailsSupport")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "The server has support for completion item label\ndetails (see also `CompletionItemLabelDetails`) when\nreceiving a completion item in a resolve call.\n\n@since 3.17.0")
				     (since . "3.17.0"))])))
		(optional . t)
		(documentation .
			       "The server supports the following `CompletionItem` specific\ncapabilities.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation . "Completion options."))
 ((name . "HoverOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation . "Hover options."))
 ((name . "SignatureHelpContext")
  (properties .
	      [((name . "triggerKind")
		(type (kind . "reference")
		      (name . "SignatureHelpTriggerKind"))
		(documentation .
			       "Action that caused signature help to be triggered."))
	       ((name . "triggerCharacter")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "Character that caused signature help to be triggered.\n\nThis is undefined when `triggerKind !== SignatureHelpTriggerKind.TriggerCharacter`"))
	       ((name . "isRetrigger")
		(type (kind . "base") (name . "boolean"))
		(documentation .
			       "`true` if signature help was already showing when it was triggered.\n\nRetriggers occurs when the signature help is already active and can be caused by actions such as\ntyping a trigger character, a cursor move, or document content changes."))
	       ((name . "activeSignatureHelp")
		(type (kind . "reference") (name . "SignatureHelp"))
		(optional . t)
		(documentation .
			       "The currently active `SignatureHelp`.\n\nThe `activeSignatureHelp` has its `SignatureHelp.activeSignature` field updated based on\nthe user navigating through available signatures."))])
  (documentation .
		 "Additional information about the context in which a signature help request was triggered.\n\n@since 3.15.0")
  (since . "3.15.0"))
 ((name . "SignatureInformation")
  (properties .
	      [((name . "label")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The label of this signature. Will be shown in\nthe UI."))
	       ((name . "documentation")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "reference")
			       (name . "MarkupContent"))]))
		(optional . t)
		(documentation .
			       "The human-readable doc-comment of this signature. Will be shown\nin the UI but can be omitted."))
	       ((name . "parameters")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "ParameterInformation")))
		(optional . t)
		(documentation . "The parameters of this signature."))
	       ((name . "activeParameter")
		(type (kind . "base") (name . "uinteger"))
		(optional . t)
		(documentation .
			       "The index of the active parameter.\n\nIf provided, this is used in place of `SignatureHelp.activeParameter`.\n\n@since 3.16.0")
		(since . "3.16.0"))])
  (documentation .
		 "Represents the signature of something callable. A signature\ncan have a label, like a function-name, a doc-comment, and\na set of parameters."))
 ((name . "SignatureHelpOptions")
  (properties .
	      [((name . "triggerCharacters")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(optional . t)
		(documentation .
			       "List of characters that trigger signature help automatically."))
	       ((name . "retriggerCharacters")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(optional . t)
		(documentation .
			       "List of characters that re-trigger signature help.\n\nThese trigger characters are only active when signature help is already showing. All trigger characters\nare also counted as re-trigger characters.\n\n@since 3.15.0")
		(since . "3.15.0"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Server Capabilities for a [SignatureHelpRequest](#SignatureHelpRequest)."))
 ((name . "DefinitionOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Server Capabilities for a [DefinitionRequest](#DefinitionRequest)."))
 ((name . "ReferenceContext")
  (properties .
	      [((name . "includeDeclaration")
		(type (kind . "base") (name . "boolean"))
		(documentation .
			       "Include the declaration of the current symbol."))])
  (documentation .
		 "Value-object that contains additional information when\nrequesting references."))
 ((name . "ReferenceOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation . "Reference options."))
 ((name . "DocumentHighlightOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Provider options for a [DocumentHighlightRequest](#DocumentHighlightRequest)."))
 ((name . "BaseSymbolInformation")
  (properties .
	      [((name . "name")
		(type (kind . "base") (name . "string"))
		(documentation . "The name of this symbol."))
	       ((name . "kind")
		(type (kind . "reference") (name . "SymbolKind"))
		(documentation . "The kind of this symbol."))
	       ((name . "tags")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "SymbolTag")))
		(optional . t)
		(documentation .
			       "Tags for this symbol.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "containerName")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "The name of the symbol containing this symbol. This information is for\nuser interface purposes (e.g. to render a qualifier in the user interface\nif necessary). It can't be used to re-infer a hierarchy for the document\nsymbols."))])
  (documentation . "A base for all symbol information."))
 ((name . "DocumentSymbolOptions")
  (properties .
	      [((name . "label")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "A human-readable string that is shown when multiple outlines trees\nare shown for the same document.\n\n@since 3.16.0")
		(since . "3.16.0"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Provider options for a [DocumentSymbolRequest](#DocumentSymbolRequest)."))
 ((name . "CodeActionContext")
  (properties .
	      [((name . "diagnostics")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "Diagnostic")))
		(documentation .
			       "An array of diagnostics known on the client side overlapping the range provided to the\n`textDocument/codeAction` request. They are provided so that the server knows which\nerrors are currently presented to the user for the given range. There is no guarantee\nthat these accurately reflect the error state of the resource. The primary parameter\nto compute code actions is the provided range."))
	       ((name . "only")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "CodeActionKind")))
		(optional . t)
		(documentation .
			       "Requested kind of actions to return.\n\nActions not of this kind are filtered out by the client before being shown. So servers\ncan omit computing them."))
	       ((name . "triggerKind")
		(type (kind . "reference")
		      (name . "CodeActionTriggerKind"))
		(optional . t)
		(documentation .
			       "The reason why code actions were requested.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (documentation .
		 "Contains additional diagnostic information about the context in which\na [code action](#CodeActionProvider.provideCodeActions) is run."))
 ((name . "CodeActionOptions")
  (properties .
	      [((name . "codeActionKinds")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "CodeActionKind")))
		(optional . t)
		(documentation .
			       "CodeActionKinds that this server may return.\n\nThe list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server\nmay list out every specific kind they provide."))
	       ((name . "resolveProvider")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The server provides support to resolve additional\ninformation for a code action.\n\n@since 3.16.0")
		(since . "3.16.0"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Provider options for a [CodeActionRequest](#CodeActionRequest)."))
 ((name . "WorkspaceSymbolOptions")
  (properties .
	      [((name . "resolveProvider")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The server provides support to resolve additional\ninformation for a workspace symbol.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Server capabilities for a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest)."))
 ((name . "CodeLensOptions")
  (properties .
	      [((name . "resolveProvider")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Code lens has a resolve provider as well."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Code Lens provider options of a [CodeLensRequest](#CodeLensRequest)."))
 ((name . "DocumentLinkOptions")
  (properties .
	      [((name . "resolveProvider")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Document links have a resolve provider as well."))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Provider options for a [DocumentLinkRequest](#DocumentLinkRequest)."))
 ((name . "FormattingOptions")
  (properties .
	      [((name . "tabSize")
		(type (kind . "base") (name . "uinteger"))
		(documentation . "Size of a tab in spaces."))
	       ((name . "insertSpaces")
		(type (kind . "base") (name . "boolean"))
		(documentation . "Prefer spaces over tabs."))
	       ((name . "trimTrailingWhitespace")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Trim trailing whitespace on a line.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "insertFinalNewline")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Insert a newline character at the end of the file if one does not exist.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "trimFinalNewlines")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Trim all newlines after the final newline at the end of the file.\n\n@since 3.15.0")
		(since . "3.15.0"))])
  (documentation .
		 "Value-object describing what options formatting should use."))
 ((name . "DocumentFormattingOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Provider options for a [DocumentFormattingRequest](#DocumentFormattingRequest)."))
 ((name . "DocumentRangeFormattingOptions") (properties . [])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Provider options for a [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest)."))
 ((name . "DocumentOnTypeFormattingOptions")
  (properties .
	      [((name . "firstTriggerCharacter")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "A character on which formatting should be triggered, like `{`."))
	       ((name . "moreTriggerCharacter")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(optional . t)
		(documentation . "More trigger characters."))])
  (documentation .
		 "Provider options for a [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest)."))
 ((name . "RenameOptions")
  (properties .
	      [((name . "prepareProvider")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Renames should be checked and tested before being executed.\n\n@since version 3.12.0")
		(since . "version 3.12.0"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "Provider options for a [RenameRequest](#RenameRequest)."))
 ((name . "ExecuteCommandOptions")
  (properties .
	      [((name . "commands")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(documentation .
			       "The commands to be executed on the server"))])
  (mixins .
	  [((kind . "reference") (name . "WorkDoneProgressOptions"))])
  (documentation .
		 "The server capabilities of a [ExecuteCommandRequest](#ExecuteCommandRequest)."))
 ((name . "SemanticTokensLegend")
  (properties .
	      [((name . "tokenTypes")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(documentation . "The token types a server uses."))
	       ((name . "tokenModifiers")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(documentation . "The token modifiers a server uses."))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "OptionalVersionedTextDocumentIdentifier")
  (properties .
	      [((name . "version")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "integer"))
			      ((kind . "base") (name . "null"))]))
		(documentation .
			       "The version number of this document. If a versioned text document identifier\nis sent from the server to the client and the file is not open in the editor\n(the server has not received an open notification before) the server can send\n`null` to indicate that the version is unknown and the content on disk is the\ntruth (as specified with document content ownership)."))])
  (extends .
	   [((kind . "reference") (name . "TextDocumentIdentifier"))])
  (documentation .
		 "A text document identifier to optionally denote a specific version of a text document."))
 ((name . "AnnotatedTextEdit")
  (properties .
	      [((name . "annotationId")
		(type (kind . "reference")
		      (name . "ChangeAnnotationIdentifier"))
		(documentation .
			       "The actual identifier of the change annotation"))])
  (extends . [((kind . "reference") (name . "TextEdit"))])
  (documentation .
		 "A special text edit with an additional change annotation.\n\n@since 3.16.0.")
  (since . "3.16.0."))
 ((name . "ResourceOperation")
  (properties .
	      [((name . "kind")
		(type (kind . "base") (name . "string"))
		(documentation . "The resource operation kind."))
	       ((name . "annotationId")
		(type (kind . "reference")
		      (name . "ChangeAnnotationIdentifier"))
		(optional . t)
		(documentation .
			       "An optional annotation identifier describing the operation.\n\n@since 3.16.0")
		(since . "3.16.0"))])
  (documentation . "A generic resource operation."))
 ((name . "CreateFileOptions")
  (properties .
	      [((name . "overwrite")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Overwrite existing file. Overwrite wins over `ignoreIfExists`"))
	       ((name . "ignoreIfExists")
		(type (kind . "base") (name . "boolean"))
		(optional . t) (documentation . "Ignore if exists."))])
  (documentation . "Options to create a file."))
 ((name . "RenameFileOptions")
  (properties .
	      [((name . "overwrite")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Overwrite target if existing. Overwrite wins over `ignoreIfExists`"))
	       ((name . "ignoreIfExists")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation . "Ignores if target exists."))])
  (documentation . "Rename file options"))
 ((name . "DeleteFileOptions")
  (properties .
	      [((name . "recursive")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Delete the content recursively if a folder is denoted."))
	       ((name . "ignoreIfNotExists")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Ignore the operation if the file doesn't exist."))])
  (documentation . "Delete file options"))
 ((name . "FileOperationPattern")
  (properties .
	      [((name . "glob")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The glob pattern to match. Glob patterns can have the following syntax:\n- `*` to match one or more characters in a path segment\n- `?` to match on one character in a path segment\n- `**` to match any number of path segments, including none\n- `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)\n- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)\n- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)"))
	       ((name . "matches")
		(type (kind . "reference")
		      (name . "FileOperationPatternKind"))
		(optional . t)
		(documentation .
			       "Whether to match files or folders with this pattern.\n\nMatches both if undefined."))
	       ((name . "options")
		(type (kind . "reference")
		      (name . "FileOperationPatternOptions"))
		(optional . t)
		(documentation .
			       "Additional options used during matching."))])
  (documentation .
		 "A pattern to describe in which file operation requests or notifications\nthe server is interested in receiving.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "WorkspaceFullDocumentDiagnosticReport")
  (properties .
	      [((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation .
			       "The URI for which diagnostic information is reported."))
	       ((name . "version")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "integer"))
			      ((kind . "base") (name . "null"))]))
		(documentation .
			       "The version number for which the diagnostics are reported.\nIf the document is not marked as open `null` can be provided."))])
  (extends .
	   [((kind . "reference")
	     (name . "FullDocumentDiagnosticReport"))])
  (documentation .
		 "A full document diagnostic report for a workspace diagnostic result.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "WorkspaceUnchangedDocumentDiagnosticReport")
  (properties .
	      [((name . "uri")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation .
			       "The URI for which diagnostic information is reported."))
	       ((name . "version")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "integer"))
			      ((kind . "base") (name . "null"))]))
		(documentation .
			       "The version number for which the diagnostics are reported.\nIf the document is not marked as open `null` can be provided."))])
  (extends .
	   [((kind . "reference")
	     (name . "UnchangedDocumentDiagnosticReport"))])
  (documentation .
		 "An unchanged document diagnostic report for a workspace diagnostic result.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "LSPObject") (properties . [])
  (documentation . "LSP object definition.\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "NotebookCell")
  (properties .
	      [((name . "kind")
		(type (kind . "reference") (name . "NotebookCellKind"))
		(documentation . "The cell's kind"))
	       ((name . "document")
		(type (kind . "base") (name . "DocumentUri"))
		(documentation .
			       "The URI of the cell's text document\ncontent."))
	       ((name . "metadata")
		(type (kind . "reference") (name . "LSPObject"))
		(optional . t)
		(documentation .
			       "Additional metadata stored with the cell.\n\nNote: should always be an object literal (e.g. LSPObject)"))
	       ((name . "executionSummary")
		(type (kind . "reference") (name . "ExecutionSummary"))
		(optional . t)
		(documentation .
			       "Additional execution summary information\nif supported by the client."))])
  (documentation .
		 "A notebook cell.\n\nA cell's document URI must be unique across ALL notebook\ncells and can therefore be used to uniquely identify a\nnotebook cell or the cell's text document.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "NotebookCellArrayChange")
  (properties .
	      [((name . "start")
		(type (kind . "base") (name . "uinteger"))
		(documentation .
			       "The start oftest of the cell that changed."))
	       ((name . "deleteCount")
		(type (kind . "base") (name . "uinteger"))
		(documentation . "The deleted cells"))
	       ((name . "cells")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "NotebookCell")))
		(optional . t)
		(documentation . "The new cells, if any"))])
  (documentation .
		 "A change describing how to move a `NotebookCell`\narray from state S to S'.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "ClientCapabilities")
  (properties .
	      [((name . "workspace")
		(type (kind . "reference")
		      (name . "WorkspaceClientCapabilities"))
		(optional . t)
		(documentation .
			       "Workspace specific client capabilities."))
	       ((name . "textDocument")
		(type (kind . "reference")
		      (name . "TextDocumentClientCapabilities"))
		(optional . t)
		(documentation .
			       "Text document specific client capabilities."))
	       ((name . "notebookDocument")
		(type (kind . "reference")
		      (name . "NotebookDocumentClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the notebook document support.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "window")
		(type (kind . "reference")
		      (name . "WindowClientCapabilities"))
		(optional . t)
		(documentation .
			       "Window specific client capabilities."))
	       ((name . "general")
		(type (kind . "reference")
		      (name . "GeneralClientCapabilities"))
		(optional . t)
		(documentation .
			       "General client capabilities.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "experimental")
		(type (kind . "reference") (name . "LSPAny"))
		(optional . t)
		(documentation . "Experimental client capabilities."))])
  (documentation . "Defines the capabilities provided by the client."))
 ((name . "TextDocumentSyncOptions")
  (properties .
	      [((name . "openClose")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Open and close notifications are sent to the server. If omitted open close notification should not\nbe sent."))
	       ((name . "change")
		(type (kind . "reference")
		      (name . "TextDocumentSyncKind"))
		(optional . t)
		(documentation .
			       "Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full\nand TextDocumentSyncKind.Incremental. If omitted it defaults to TextDocumentSyncKind.None."))
	       ((name . "willSave")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "If present will save notifications are sent to the server. If omitted the notification should not be\nsent."))
	       ((name . "willSaveWaitUntil")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "If present will save wait until requests are sent to the server. If omitted the request should not be\nsent."))
	       ((name . "save")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "boolean"))
			      ((kind . "reference")
			       (name . "SaveOptions"))]))
		(optional . t)
		(documentation .
			       "If present save notifications are sent to the server. If omitted the notification should not be\nsent."))]))
 ((name . "NotebookDocumentSyncOptions")
  (properties .
	      [((name . "notebookSelector")
		(type (kind . "array")
		      (element (kind . "or")
			       (items .
				      [((kind . "literal")
					(value
					 (properties .
						     [((name .
							     "notebook")
						       (type
							(kind . "or")
							(items .
							       [((kind
								  .
								  "base")
								 (name
								  .
								  "string"))
								((kind
								  .
								  "reference")
								 (name
								  .
								  "NotebookDocumentFilter"))]))
						       (documentation
							.
							"The notebook to be synced If a string\nvalue is provided it matches against the\nnotebook type. '*' matches every notebook."))
						      ((name . "cells")
						       (type
							(kind .
							      "array")
							(element
							 (kind .
							       "literal")
							 (value
							  (properties
							   .
							   [((name .
								   "language")
							     (type
							      (kind .
								    "base")
							      (name .
								    "string")))]))))
						       (optional . t)
						       (documentation
							.
							"The cells of the matching notebook to be synced."))])))
				       ((kind . "literal")
					(value
					 (properties .
						     [((name .
							     "notebook")
						       (type
							(kind . "or")
							(items .
							       [((kind
								  .
								  "base")
								 (name
								  .
								  "string"))
								((kind
								  .
								  "reference")
								 (name
								  .
								  "NotebookDocumentFilter"))]))
						       (optional . t)
						       (documentation
							.
							"The notebook to be synced If a string\nvalue is provided it matches against the\nnotebook type. '*' matches every notebook."))
						      ((name . "cells")
						       (type
							(kind .
							      "array")
							(element
							 (kind .
							       "literal")
							 (value
							  (properties
							   .
							   [((name .
								   "language")
							     (type
							      (kind .
								    "base")
							      (name .
								    "string")))]))))
						       (documentation
							.
							"The cells of the matching notebook to be synced."))])))])))
		(documentation . "The notebooks to be synced"))
	       ((name . "save")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether save notification should be forwarded to\nthe server. Will only be honored if mode === `notebook`."))])
  (documentation .
		 "Options specific to a notebook plus its cells\nto be synced to the server.\n\nIf a selector provides a notebook document\nfilter but no cell selector all cells of a\nmatching notebook document will be synced.\n\nIf a selector provides no notebook document\nfilter but only a cell selector all notebook\ndocument that contain at least one matching\ncell will be synced.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "NotebookDocumentSyncRegistrationOptions") (properties . [])
  (extends .
	   [((kind . "reference")
	     (name . "NotebookDocumentSyncOptions"))])
  (mixins .
	  [((kind . "reference") (name . "StaticRegistrationOptions"))])
  (documentation .
		 "Registration options specific to a notebook.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "WorkspaceFoldersServerCapabilities")
  (properties .
	      [((name . "supported")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The server has support for workspace folders"))
	       ((name . "changeNotifications")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "base") (name . "boolean"))]))
		(optional . t)
		(documentation .
			       "Whether the server wants to receive workspace folder\nchange notifications.\n\nIf a string is provided the string is treated as an ID\nunder which the notification is registered on the client\nside. The ID can be used to unregister for these events\nusing the `client/unregisterCapability` request."))]))
 ((name . "FileOperationOptions")
  (properties .
	      [((name . "didCreate")
		(type (kind . "reference")
		      (name . "FileOperationRegistrationOptions"))
		(optional . t)
		(documentation .
			       "The server is interested in receiving didCreateFiles notifications."))
	       ((name . "willCreate")
		(type (kind . "reference")
		      (name . "FileOperationRegistrationOptions"))
		(optional . t)
		(documentation .
			       "The server is interested in receiving willCreateFiles requests."))
	       ((name . "didRename")
		(type (kind . "reference")
		      (name . "FileOperationRegistrationOptions"))
		(optional . t)
		(documentation .
			       "The server is interested in receiving didRenameFiles notifications."))
	       ((name . "willRename")
		(type (kind . "reference")
		      (name . "FileOperationRegistrationOptions"))
		(optional . t)
		(documentation .
			       "The server is interested in receiving willRenameFiles requests."))
	       ((name . "didDelete")
		(type (kind . "reference")
		      (name . "FileOperationRegistrationOptions"))
		(optional . t)
		(documentation .
			       "The server is interested in receiving didDeleteFiles file notifications."))
	       ((name . "willDelete")
		(type (kind . "reference")
		      (name . "FileOperationRegistrationOptions"))
		(optional . t)
		(documentation .
			       "The server is interested in receiving willDeleteFiles file requests."))])
  (documentation .
		 "Options for notifications/requests for user operations on files.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "CodeDescription")
  (properties .
	      [((name . "href") (type (kind . "base") (name . "URI"))
		(documentation .
			       "An URI to open with more information about the diagnostic error."))])
  (documentation .
		 "Structure to capture a description for an error code.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "DiagnosticRelatedInformation")
  (properties .
	      [((name . "location")
		(type (kind . "reference") (name . "Location"))
		(documentation .
			       "The location of this related diagnostic information."))
	       ((name . "message")
		(type (kind . "base") (name . "string"))
		(documentation .
			       "The message of this related diagnostic information."))])
  (documentation .
		 "Represents a related message and source code location for a diagnostic. This should be\nused to point to code locations that cause or related to a diagnostics, e.g when duplicating\na symbol in a scope."))
 ((name . "ParameterInformation")
  (properties .
	      [((name . "label")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "tuple")
			       (items .
				      [((kind . "base")
					(name . "uinteger"))
				       ((kind . "base")
					(name . "uinteger"))]))]))
		(documentation .
			       "The label of this parameter information.\n\nEither a string or an inclusive start and exclusive end offsets within its containing\nsignature label. (see SignatureInformation.label). The offsets are based on a UTF-16\nstring representation as `Position` and `Range` does.\n\n*Note*: a label of type string should be a substring of its containing signature label.\nIts intended use case is to highlight the parameter label part in the `SignatureInformation.label`."))
	       ((name . "documentation")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "reference")
			       (name . "MarkupContent"))]))
		(optional . t)
		(documentation .
			       "The human-readable doc-comment of this parameter. Will be shown\nin the UI but can be omitted."))])
  (documentation .
		 "Represents a parameter of a callable-signature. A parameter can\nhave a label and a doc-comment."))
 ((name . "NotebookCellTextDocumentFilter")
  (properties .
	      [((name . "notebook")
		(type (kind . "or")
		      (items .
			     [((kind . "base") (name . "string"))
			      ((kind . "reference")
			       (name . "NotebookDocumentFilter"))]))
		(documentation .
			       "A filter that matches against the notebook\ncontaining the notebook cell. If a string\nvalue is provided it matches against the\nnotebook type. '*' matches every notebook."))
	       ((name . "language")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation .
			       "A language id like `python`.\n\nWill be matched against the language id of the\nnotebook cell document. '*' matches every language."))])
  (documentation .
		 "A notebook cell text document filter denotes a cell text\ndocument by different properties.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "FileOperationPatternOptions")
  (properties .
	      [((name . "ignoreCase")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The pattern should be matched ignoring casing."))])
  (documentation .
		 "Matching options for the file operation pattern.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "ExecutionSummary")
  (properties .
	      [((name . "executionOrder")
		(type (kind . "base") (name . "uinteger"))
		(documentation .
			       "A strict monotonically increasing value\nindicating the execution order of a cell\ninside a notebook."))
	       ((name . "success")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the execution was successful or\nnot if known by the client."))]))
 ((name . "WorkspaceClientCapabilities")
  (properties .
	      [((name . "applyEdit")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports applying batch edits\nto the workspace by supporting the request\n'workspace/applyEdit'"))
	       ((name . "workspaceEdit")
		(type (kind . "reference")
		      (name . "WorkspaceEditClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to `WorkspaceEdit`s."))
	       ((name . "didChangeConfiguration")
		(type (kind . "reference")
		      (name .
			    "DidChangeConfigurationClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `workspace/didChangeConfiguration` notification."))
	       ((name . "didChangeWatchedFiles")
		(type (kind . "reference")
		      (name .
			    "DidChangeWatchedFilesClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `workspace/didChangeWatchedFiles` notification."))
	       ((name . "symbol")
		(type (kind . "reference")
		      (name . "WorkspaceSymbolClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `workspace/symbol` request."))
	       ((name . "executeCommand")
		(type (kind . "reference")
		      (name . "ExecuteCommandClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `workspace/executeCommand` request."))
	       ((name . "workspaceFolders")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client has support for workspace folders.\n\n@since 3.6.0")
		(since . "3.6.0"))
	       ((name . "configuration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports `workspace/configuration` requests.\n\n@since 3.6.0")
		(since . "3.6.0"))
	       ((name . "semanticTokens")
		(type (kind . "reference")
		      (name .
			    "SemanticTokensWorkspaceClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the semantic token requests scoped to the\nworkspace.\n\n@since 3.16.0.")
		(since . "3.16.0."))
	       ((name . "codeLens")
		(type (kind . "reference")
		      (name . "CodeLensWorkspaceClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the code lens requests scoped to the\nworkspace.\n\n@since 3.16.0.")
		(since . "3.16.0."))
	       ((name . "fileOperations")
		(type (kind . "reference")
		      (name . "FileOperationClientCapabilities"))
		(optional . t)
		(documentation .
			       "The client has support for file notifications/requests for user operations on files.\n\nSince 3.16.0"))
	       ((name . "inlineValue")
		(type (kind . "reference")
		      (name . "InlineValueWorkspaceClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the inline values requests scoped to the\nworkspace.\n\n@since 3.17.0.")
		(since . "3.17.0."))
	       ((name . "inlayHint")
		(type (kind . "reference")
		      (name . "InlayHintWorkspaceClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the inlay hint requests scoped to the\nworkspace.\n\n@since 3.17.0.")
		(since . "3.17.0."))
	       ((name . "diagnostics")
		(type (kind . "reference")
		      (name . "DiagnosticWorkspaceClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the diagnostic requests scoped to the\nworkspace.\n\n@since 3.17.0.")
		(since . "3.17.0."))])
  (documentation . "Workspace specific client capabilities."))
 ((name . "TextDocumentClientCapabilities")
  (properties .
	      [((name . "synchronization")
		(type (kind . "reference")
		      (name . "TextDocumentSyncClientCapabilities"))
		(optional . t)
		(documentation .
			       "Defines which synchronization capabilities the client supports."))
	       ((name . "completion")
		(type (kind . "reference")
		      (name . "CompletionClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/completion` request."))
	       ((name . "hover")
		(type (kind . "reference")
		      (name . "HoverClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/hover` request."))
	       ((name . "signatureHelp")
		(type (kind . "reference")
		      (name . "SignatureHelpClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/signatureHelp` request."))
	       ((name . "declaration")
		(type (kind . "reference")
		      (name . "DeclarationClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/declaration` request.\n\n@since 3.14.0")
		(since . "3.14.0"))
	       ((name . "definition")
		(type (kind . "reference")
		      (name . "DefinitionClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/definition` request."))
	       ((name . "typeDefinition")
		(type (kind . "reference")
		      (name . "TypeDefinitionClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/typeDefinition` request.\n\n@since 3.6.0")
		(since . "3.6.0"))
	       ((name . "implementation")
		(type (kind . "reference")
		      (name . "ImplementationClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/implementation` request.\n\n@since 3.6.0")
		(since . "3.6.0"))
	       ((name . "references")
		(type (kind . "reference")
		      (name . "ReferenceClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/references` request."))
	       ((name . "documentHighlight")
		(type (kind . "reference")
		      (name . "DocumentHighlightClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/documentHighlight` request."))
	       ((name . "documentSymbol")
		(type (kind . "reference")
		      (name . "DocumentSymbolClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/documentSymbol` request."))
	       ((name . "codeAction")
		(type (kind . "reference")
		      (name . "CodeActionClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/codeAction` request."))
	       ((name . "codeLens")
		(type (kind . "reference")
		      (name . "CodeLensClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/codeLens` request."))
	       ((name . "documentLink")
		(type (kind . "reference")
		      (name . "DocumentLinkClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/documentLink` request."))
	       ((name . "colorProvider")
		(type (kind . "reference")
		      (name . "DocumentColorClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/documentColor` and the\n`textDocument/colorPresentation` request.\n\n@since 3.6.0")
		(since . "3.6.0"))
	       ((name . "formatting")
		(type (kind . "reference")
		      (name . "DocumentFormattingClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/formatting` request."))
	       ((name . "rangeFormatting")
		(type (kind . "reference")
		      (name .
			    "DocumentRangeFormattingClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/rangeFormatting` request."))
	       ((name . "onTypeFormatting")
		(type (kind . "reference")
		      (name .
			    "DocumentOnTypeFormattingClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/onTypeFormatting` request."))
	       ((name . "rename")
		(type (kind . "reference")
		      (name . "RenameClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/rename` request."))
	       ((name . "foldingRange")
		(type (kind . "reference")
		      (name . "FoldingRangeClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/foldingRange` request.\n\n@since 3.10.0")
		(since . "3.10.0"))
	       ((name . "selectionRange")
		(type (kind . "reference")
		      (name . "SelectionRangeClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/selectionRange` request.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "publishDiagnostics")
		(type (kind . "reference")
		      (name . "PublishDiagnosticsClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/publishDiagnostics` notification."))
	       ((name . "callHierarchy")
		(type (kind . "reference")
		      (name . "CallHierarchyClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the various call hierarchy requests.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "semanticTokens")
		(type (kind . "reference")
		      (name . "SemanticTokensClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the various semantic token request.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "linkedEditingRange")
		(type (kind . "reference")
		      (name . "LinkedEditingRangeClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/linkedEditingRange` request.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "moniker")
		(type (kind . "reference")
		      (name . "MonikerClientCapabilities"))
		(optional . t)
		(documentation .
			       "Client capabilities specific to the `textDocument/moniker` request.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "typeHierarchy")
		(type (kind . "reference")
		      (name . "TypeHierarchyClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the various type hierarchy requests.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "inlineValue")
		(type (kind . "reference")
		      (name . "InlineValueClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/inlineValue` request.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "inlayHint")
		(type (kind . "reference")
		      (name . "InlayHintClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `textDocument/inlayHint` request.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "diagnostic")
		(type (kind . "reference")
		      (name . "DiagnosticClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the diagnostic pull model.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (documentation . "Text document specific client capabilities."))
 ((name . "NotebookDocumentClientCapabilities")
  (properties .
	      [((name . "synchronization")
		(type (kind . "reference")
		      (name . "NotebookDocumentSyncClientCapabilities"))
		(documentation .
			       "Capabilities specific to notebook document synchronization\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (documentation .
		 "Capabilities specific to the notebook document support.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "WindowClientCapabilities")
  (properties .
	      [((name . "workDoneProgress")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "It indicates whether the client supports server initiated\nprogress using the `window/workDoneProgress/create` request.\n\nThe capability also controls Whether client supports handling\nof progress notifications. If set servers are allowed to report a\n`workDoneProgress` property in the request specific server\ncapabilities.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "showMessage")
		(type (kind . "reference")
		      (name . "ShowMessageRequestClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the showMessage request.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "showDocument")
		(type (kind . "reference")
		      (name . "ShowDocumentClientCapabilities"))
		(optional . t)
		(documentation .
			       "Capabilities specific to the showDocument request.\n\n@since 3.16.0")
		(since . "3.16.0"))]))
 ((name . "GeneralClientCapabilities")
  (properties .
	      [((name . "staleRequestSupport")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "cancel")
				     (type (kind . "base")
					   (name . "boolean"))
				     (documentation .
						    "The client will actively cancel the request."))
				    ((name . "retryOnContentModified")
				     (type (kind . "array")
					   (element (kind . "base")
						    (name . "string")))
				     (documentation .
						    "The list of requests for which the client\nwill retry the request if it receives a\nresponse with error code `ContentModified`"))])))
		(optional . t)
		(documentation .
			       "Client capability that signals how the client\nhandles stale requests (e.g. a request\nfor which the client will not process the response\nanymore since the information is outdated).\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "regularExpressions")
		(type (kind . "reference")
		      (name . "RegularExpressionsClientCapabilities"))
		(optional . t)
		(documentation .
			       "Client capabilities specific to regular expressions.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "markdown")
		(type (kind . "reference")
		      (name . "MarkdownClientCapabilities"))
		(optional . t)
		(documentation .
			       "Client capabilities specific to the client's markdown parser.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "positionEncodings")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "PositionEncodingKind")))
		(optional . t)
		(documentation .
			       "The position encodings supported by the client. Client and server\nhave to agree on the same position encoding to ensure that offsets\n(e.g. character position in a line) are interpreted the same on both\nsides.\n\nTo keep the protocol backwards compatible the following applies: if\nthe value 'utf-16' is missing from the array of position encodings\nservers can assume that the client supports UTF-16. UTF-16 is\ntherefore a mandatory encoding.\n\nIf omitted it defaults to ['utf-16'].\n\nImplementation considerations: since the conversion from one encoding\ninto another requires the content of the file / line the conversion\nis best done where the file is read which is usually on the server\nside.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (documentation . "General client capabilities.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "RelativePattern")
  (properties .
	      [((name . "baseUri")
		(type (kind . "or")
		      (items .
			     [((kind . "reference")
			       (name . "WorkspaceFolder"))
			      ((kind . "base") (name . "URI"))]))
		(documentation .
			       "A workspace folder or a base URI to which this pattern will be matched\nagainst relatively."))
	       ((name . "pattern")
		(type (kind . "reference") (name . "Pattern"))
		(documentation . "The actual glob pattern;"))])
  (documentation .
		 "A relative pattern is a helper to construct glob patterns that are matched\nrelatively to a base URI. The common value for a `baseUri` is a workspace\nfolder root, but it can be another absolute URI as well.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "WorkspaceEditClientCapabilities")
  (properties .
	      [((name . "documentChanges")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports versioned document changes in `WorkspaceEdit`s"))
	       ((name . "resourceOperations")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "ResourceOperationKind")))
		(optional . t)
		(documentation .
			       "The resource operations the client supports. Clients should at least\nsupport 'create', 'rename' and 'delete' files and folders.\n\n@since 3.13.0")
		(since . "3.13.0"))
	       ((name . "failureHandling")
		(type (kind . "reference")
		      (name . "FailureHandlingKind"))
		(optional . t)
		(documentation .
			       "The failure handling strategy of a client if applying the workspace edit\nfails.\n\n@since 3.13.0")
		(since . "3.13.0"))
	       ((name . "normalizesLineEndings")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client normalizes line endings to the client specific\nsetting.\nIf set to `true` the client will normalize line ending characters\nin a workspace edit to the client-specified new line\ncharacter.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "changeAnnotationSupport")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "groupsOnLabel")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "Whether the client groups edits with equal labels into tree nodes,\nfor instance all edits labelled with \"Changes in Strings\" would\nbe a tree node."))])))
		(optional . t)
		(documentation .
			       "Whether the client in general supports change annotations on text edits,\ncreate file, rename file and delete file changes.\n\n@since 3.16.0")
		(since . "3.16.0"))]))
 ((name . "DidChangeConfigurationClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Did change configuration notification supports dynamic registration."))]))
 ((name . "DidChangeWatchedFilesClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Did change watched files notification supports dynamic registration. Please note\nthat the current protocol doesn't support static configuration for file changes\nfrom the server side."))
	       ((name . "relativePatternSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client has support for {@link  RelativePattern relative pattern}\nor not.\n\n@since 3.17.0")
		(since . "3.17.0"))]))
 ((name . "WorkspaceSymbolClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Symbol request supports dynamic registration."))
	       ((name . "symbolKind")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "valueSet")
				     (type (kind . "array")
					   (element
					    (kind . "reference")
					    (name . "SymbolKind")))
				     (optional . t)
				     (documentation .
						    "The symbol kind values the client supports. When this\nproperty exists the client also guarantees that it will\nhandle values outside its set gracefully and falls back\nto a default value when unknown.\n\nIf this property is not present the client only supports\nthe symbol kinds from `File` to `Array` as defined in\nthe initial version of the protocol."))])))
		(optional . t)
		(documentation .
			       "Specific capabilities for the `SymbolKind` in the `workspace/symbol` request."))
	       ((name . "tagSupport")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "valueSet")
				     (type (kind . "array")
					   (element
					    (kind . "reference")
					    (name . "SymbolTag")))
				     (documentation .
						    "The tags supported by the client."))])))
		(optional . t)
		(documentation .
			       "The client supports tags on `SymbolInformation`.\nClients supporting tags have to handle unknown tags gracefully.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "resolveSupport")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "properties")
				     (type (kind . "array")
					   (element (kind . "base")
						    (name . "string")))
				     (documentation .
						    "The properties that a client can resolve lazily. Usually\n`location.range`"))])))
		(optional . t)
		(documentation .
			       "The client support partial workspace symbols. The client will send the\nrequest `workspaceSymbol/resolve` to the server to resolve additional\nproperties.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (documentation .
		 "Client capabilities for a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest)."))
 ((name . "ExecuteCommandClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Execute command supports dynamic registration."))])
  (documentation .
		 "The client capabilities of a [ExecuteCommandRequest](#ExecuteCommandRequest)."))
 ((name . "SemanticTokensWorkspaceClientCapabilities")
  (properties .
	      [((name . "refreshSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client implementation supports a refresh request sent from\nthe server to the client.\n\nNote that this event is global and will force the client to refresh all\nsemantic tokens currently shown. It should be used with absolute care\nand is useful for situation where a server for example detects a project\nwide change that requires such a calculation."))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "CodeLensWorkspaceClientCapabilities")
  (properties .
	      [((name . "refreshSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client implementation supports a refresh request sent from the\nserver to the client.\n\nNote that this event is global and will force the client to refresh all\ncode lenses currently shown. It should be used with absolute care and is\nuseful for situation where a server for example detect a project wide\nchange that requires such a calculation."))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "FileOperationClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client supports dynamic registration for file requests/notifications."))
	       ((name . "didCreate")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client has support for sending didCreateFiles notifications."))
	       ((name . "willCreate")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client has support for sending willCreateFiles requests."))
	       ((name . "didRename")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client has support for sending didRenameFiles notifications."))
	       ((name . "willRename")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client has support for sending willRenameFiles requests."))
	       ((name . "didDelete")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client has support for sending didDeleteFiles notifications."))
	       ((name . "willDelete")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client has support for sending willDeleteFiles requests."))])
  (documentation .
		 "Capabilities relating to events from file operations by the user in the client.\n\nThese events do not come from the file system, they come from user operations\nlike renaming a file in the UI.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "InlineValueWorkspaceClientCapabilities")
  (properties .
	      [((name . "refreshSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client implementation supports a refresh request sent from the\nserver to the client.\n\nNote that this event is global and will force the client to refresh all\ninline values currently shown. It should be used with absolute care and is\nuseful for situation where a server for example detects a project wide\nchange that requires such a calculation."))])
  (documentation .
		 "Client workspace capabilities specific to inline values.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlayHintWorkspaceClientCapabilities")
  (properties .
	      [((name . "refreshSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client implementation supports a refresh request sent from\nthe server to the client.\n\nNote that this event is global and will force the client to refresh all\ninlay hints currently shown. It should be used with absolute care and\nis useful for situation where a server for example detects a project wide\nchange that requires such a calculation."))])
  (documentation .
		 "Client workspace capabilities specific to inlay hints.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DiagnosticWorkspaceClientCapabilities")
  (properties .
	      [((name . "refreshSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client implementation supports a refresh request sent from\nthe server to the client.\n\nNote that this event is global and will force the client to refresh all\npulled diagnostics currently shown. It should be used with absolute care and\nis useful for situation where a server for example detects a project wide\nchange that requires such a calculation."))])
  (documentation .
		 "Workspace client capabilities specific to diagnostic pull requests.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "TextDocumentSyncClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether text document synchronization supports dynamic registration."))
	       ((name . "willSave")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports sending will save notifications."))
	       ((name . "willSaveWaitUntil")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports sending a will save request and\nwaits for a response providing text edits which will\nbe applied to the document before it is saved."))
	       ((name . "didSave")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports did save notifications."))]))
 ((name . "CompletionClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether completion supports dynamic registration."))
	       ((name . "completionItem")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "snippetSupport")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "Client supports snippets as insert text.\n\nA snippet can define tab stops and placeholders with `$1`, `$2`\nand `${3:foo}`. `$0` defines the final tab stop, it defaults to\nthe end of the snippet. Placeholders with equal identifiers are linked,\nthat is typing in one will update others too."))
				    ((name . "commitCharactersSupport")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "Client supports commit characters on a completion item."))
				    ((name . "documentationFormat")
				     (type (kind . "array")
					   (element
					    (kind . "reference")
					    (name . "MarkupKind")))
				     (optional . t)
				     (documentation .
						    "Client supports the following content formats for the documentation\nproperty. The order describes the preferred format of the client."))
				    ((name . "deprecatedSupport")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "Client supports the deprecated property on a completion item."))
				    ((name . "preselectSupport")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "Client supports the preselect property on a completion item."))
				    ((name . "tagSupport")
				     (type (kind . "literal")
					   (value
					    (properties .
							[((name .
								"valueSet")
							  (type
							   (kind .
								 "array")
							   (element
							    (kind .
								  "reference")
							    (name .
								  "CompletionItemTag")))
							  (documentation
							   .
							   "The tags supported by the client."))])))
				     (optional . t)
				     (documentation .
						    "Client supports the tag property on a completion item. Clients supporting\ntags have to handle unknown tags gracefully. Clients especially need to\npreserve unknown tags when sending a completion item back to the server in\na resolve call.\n\n@since 3.15.0")
				     (since . "3.15.0"))
				    ((name . "insertReplaceSupport")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "Client support insert replace edit to control different behavior if a\ncompletion item is inserted in the text or should replace text.\n\n@since 3.16.0")
				     (since . "3.16.0"))
				    ((name . "resolveSupport")
				     (type (kind . "literal")
					   (value
					    (properties .
							[((name .
								"properties")
							  (type
							   (kind .
								 "array")
							   (element
							    (kind .
								  "base")
							    (name .
								  "string")))
							  (documentation
							   .
							   "The properties that a client can resolve lazily."))])))
				     (optional . t)
				     (documentation .
						    "Indicates which properties a client can resolve lazily on a completion\nitem. Before version 3.16.0 only the predefined properties `documentation`\nand `details` could be resolved lazily.\n\n@since 3.16.0")
				     (since . "3.16.0"))
				    ((name . "insertTextModeSupport")
				     (type (kind . "literal")
					   (value
					    (properties .
							[((name .
								"valueSet")
							  (type
							   (kind .
								 "array")
							   (element
							    (kind .
								  "reference")
							    (name .
								  "InsertTextMode"))))])))
				     (optional . t)
				     (documentation .
						    "The client supports the `insertTextMode` property on\na completion item to override the whitespace handling mode\nas defined by the client (see `insertTextMode`).\n\n@since 3.16.0")
				     (since . "3.16.0"))
				    ((name . "labelDetailsSupport")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "The client has support for completion item label\ndetails (see also `CompletionItemLabelDetails`).\n\n@since 3.17.0")
				     (since . "3.17.0"))])))
		(optional . t)
		(documentation .
			       "The client supports the following `CompletionItem` specific\ncapabilities."))
	       ((name . "completionItemKind")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "valueSet")
				     (type (kind . "array")
					   (element
					    (kind . "reference")
					    (name .
						  "CompletionItemKind")))
				     (optional . t)
				     (documentation .
						    "The completion item kind values the client supports. When this\nproperty exists the client also guarantees that it will\nhandle values outside its set gracefully and falls back\nto a default value when unknown.\n\nIf this property is not present the client only supports\nthe completion items kinds from `Text` to `Reference` as defined in\nthe initial version of the protocol."))])))
		(optional . t))
	       ((name . "insertTextMode")
		(type (kind . "reference") (name . "InsertTextMode"))
		(optional . t)
		(documentation .
			       "Defines how the client handles whitespace and indentation\nwhen accepting a completion item that uses multi line\ntext in either `insertText` or `textEdit`.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "contextSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports to send additional context information for a\n`textDocument/completion` request."))
	       ((name . "completionList")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "itemDefaults")
				     (type (kind . "array")
					   (element (kind . "base")
						    (name . "string")))
				     (optional . t)
				     (documentation .
						    "The client supports the following itemDefaults on\na completion list.\n\nThe value lists the supported property names of the\n`CompletionList.itemDefaults` object. If omitted\nno properties are supported.\n\n@since 3.17.0")
				     (since . "3.17.0"))])))
		(optional . t)
		(documentation .
			       "The client supports the following `CompletionList` specific\ncapabilities.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (documentation . "Completion client capabilities"))
 ((name . "HoverClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether hover supports dynamic registration."))
	       ((name . "contentFormat")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "MarkupKind")))
		(optional . t)
		(documentation .
			       "Client supports the following content formats for the content\nproperty. The order describes the preferred format of the client."))]))
 ((name . "SignatureHelpClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether signature help supports dynamic registration."))
	       ((name . "signatureInformation")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "documentationFormat")
				     (type (kind . "array")
					   (element
					    (kind . "reference")
					    (name . "MarkupKind")))
				     (optional . t)
				     (documentation .
						    "Client supports the following content formats for the documentation\nproperty. The order describes the preferred format of the client."))
				    ((name . "parameterInformation")
				     (type (kind . "literal")
					   (value
					    (properties .
							[((name .
								"labelOffsetSupport")
							  (type
							   (kind .
								 "base")
							   (name .
								 "boolean"))
							  (optional .
								    t)
							  (documentation
							   .
							   "The client supports processing label offsets instead of a\nsimple label string.\n\n@since 3.14.0")
							  (since .
								 "3.14.0"))])))
				     (optional . t)
				     (documentation .
						    "Client capabilities specific to parameter information."))
				    ((name . "activeParameterSupport")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "The client supports the `activeParameter` property on `SignatureInformation`\nliteral.\n\n@since 3.16.0")
				     (since . "3.16.0"))])))
		(optional . t)
		(documentation .
			       "The client supports the following `SignatureInformation`\nspecific properties."))
	       ((name . "contextSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports to send additional context information for a\n`textDocument/signatureHelp` request. A client that opts into\ncontextSupport will also support the `retriggerCharacters` on\n`SignatureHelpOptions`.\n\n@since 3.15.0")
		(since . "3.15.0"))])
  (documentation .
		 "Client Capabilities for a [SignatureHelpRequest](#SignatureHelpRequest)."))
 ((name . "DeclarationClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether declaration supports dynamic registration. If this is set to `true`\nthe client supports the new `DeclarationRegistrationOptions` return value\nfor the corresponding server capability as well."))
	       ((name . "linkSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports additional metadata in the form of declaration links."))])
  (documentation . "@since 3.14.0") (since . "3.14.0"))
 ((name . "DefinitionClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether definition supports dynamic registration."))
	       ((name . "linkSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports additional metadata in the form of definition links.\n\n@since 3.14.0")
		(since . "3.14.0"))])
  (documentation .
		 "Client Capabilities for a [DefinitionRequest](#DefinitionRequest)."))
 ((name . "TypeDefinitionClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration. If this is set to `true`\nthe client supports the new `TypeDefinitionRegistrationOptions` return value\nfor the corresponding server capability as well."))
	       ((name . "linkSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports additional metadata in the form of definition links.\n\nSince 3.14.0"))])
  (documentation . "Since 3.6.0"))
 ((name . "ImplementationClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration. If this is set to `true`\nthe client supports the new `ImplementationRegistrationOptions` return value\nfor the corresponding server capability as well."))
	       ((name . "linkSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports additional metadata in the form of definition links.\n\n@since 3.14.0")
		(since . "3.14.0"))])
  (documentation . "@since 3.6.0") (since . "3.6.0"))
 ((name . "ReferenceClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether references supports dynamic registration."))])
  (documentation .
		 "Client Capabilities for a [ReferencesRequest](#ReferencesRequest)."))
 ((name . "DocumentHighlightClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether document highlight supports dynamic registration."))])
  (documentation .
		 "Client Capabilities for a [DocumentHighlightRequest](#DocumentHighlightRequest)."))
 ((name . "DocumentSymbolClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether document symbol supports dynamic registration."))
	       ((name . "symbolKind")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "valueSet")
				     (type (kind . "array")
					   (element
					    (kind . "reference")
					    (name . "SymbolKind")))
				     (optional . t)
				     (documentation .
						    "The symbol kind values the client supports. When this\nproperty exists the client also guarantees that it will\nhandle values outside its set gracefully and falls back\nto a default value when unknown.\n\nIf this property is not present the client only supports\nthe symbol kinds from `File` to `Array` as defined in\nthe initial version of the protocol."))])))
		(optional . t)
		(documentation .
			       "Specific capabilities for the `SymbolKind` in the\n`textDocument/documentSymbol` request."))
	       ((name . "hierarchicalDocumentSymbolSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports hierarchical document symbols."))
	       ((name . "tagSupport")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "valueSet")
				     (type (kind . "array")
					   (element
					    (kind . "reference")
					    (name . "SymbolTag")))
				     (documentation .
						    "The tags supported by the client."))])))
		(optional . t)
		(documentation .
			       "The client supports tags on `SymbolInformation`. Tags are supported on\n`DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to true.\nClients supporting tags have to handle unknown tags gracefully.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "labelSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports an additional label presented in the UI when\nregistering a document symbol provider.\n\n@since 3.16.0")
		(since . "3.16.0"))])
  (documentation .
		 "Client Capabilities for a [DocumentSymbolRequest](#DocumentSymbolRequest)."))
 ((name . "CodeActionClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether code action supports dynamic registration."))
	       ((name . "codeActionLiteralSupport")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "codeActionKind")
				     (type (kind . "literal")
					   (value
					    (properties .
							[((name .
								"valueSet")
							  (type
							   (kind .
								 "array")
							   (element
							    (kind .
								  "reference")
							    (name .
								  "CodeActionKind")))
							  (documentation
							   .
							   "The code action kind values the client supports. When this\nproperty exists the client also guarantees that it will\nhandle values outside its set gracefully and falls back\nto a default value when unknown."))])))
				     (documentation .
						    "The code action kind is support with the following value\nset."))])))
		(optional . t)
		(documentation .
			       "The client support code action literals of type `CodeAction` as a valid\nresponse of the `textDocument/codeAction` request. If the property is not\nset the request can only return `Command` literals.\n\n@since 3.8.0")
		(since . "3.8.0"))
	       ((name . "isPreferredSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether code action supports the `isPreferred` property.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "disabledSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether code action supports the `disabled` property.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "dataSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether code action supports the `data` property which is\npreserved between a `textDocument/codeAction` and a\n`codeAction/resolve` request.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "resolveSupport")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "properties")
				     (type (kind . "array")
					   (element (kind . "base")
						    (name . "string")))
				     (documentation .
						    "The properties that a client can resolve lazily."))])))
		(optional . t)
		(documentation .
			       "Whether the client supports resolving additional code action\nproperties via a separate `codeAction/resolve` request.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "honorsChangeAnnotations")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client honors the change annotations in\ntext edits and resource operations returned via the\n`CodeAction#edit` property by for example presenting\nthe workspace edit in the user interface and asking\nfor confirmation.\n\n@since 3.16.0")
		(since . "3.16.0"))])
  (documentation .
		 "The Client Capabilities of a [CodeActionRequest](#CodeActionRequest)."))
 ((name . "CodeLensClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether code lens supports dynamic registration."))])
  (documentation .
		 "The client capabilities  of a [CodeLensRequest](#CodeLensRequest)."))
 ((name . "DocumentLinkClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether document link supports dynamic registration."))
	       ((name . "tooltipSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client supports the `tooltip` property on `DocumentLink`.\n\n@since 3.15.0")
		(since . "3.15.0"))])
  (documentation .
		 "The client capabilities of a [DocumentLinkRequest](#DocumentLinkRequest)."))
 ((name . "DocumentColorClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration. If this is set to `true`\nthe client supports the new `DocumentColorRegistrationOptions` return value\nfor the corresponding server capability as well."))]))
 ((name . "DocumentFormattingClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether formatting supports dynamic registration."))])
  (documentation .
		 "Client capabilities of a [DocumentFormattingRequest](#DocumentFormattingRequest)."))
 ((name . "DocumentRangeFormattingClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether range formatting supports dynamic registration."))])
  (documentation .
		 "Client capabilities of a [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest)."))
 ((name . "DocumentOnTypeFormattingClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether on type formatting supports dynamic registration."))])
  (documentation .
		 "Client capabilities of a [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest)."))
 ((name . "RenameClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether rename supports dynamic registration."))
	       ((name . "prepareSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Client supports testing for validity of rename operations\nbefore execution.\n\n@since 3.12.0")
		(since . "3.12.0"))
	       ((name . "prepareSupportDefaultBehavior")
		(type (kind . "reference")
		      (name . "PrepareSupportDefaultBehavior"))
		(optional . t)
		(documentation .
			       "Client supports the default behavior result.\n\nThe value indicates the default behavior used by the\nclient.\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "honorsChangeAnnotations")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client honors the change annotations in\ntext edits and resource operations returned via the\nrename request's workspace edit by for example presenting\nthe workspace edit in the user interface and asking\nfor confirmation.\n\n@since 3.16.0")
		(since . "3.16.0"))]))
 ((name . "FoldingRangeClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration for folding range\nproviders. If this is set to `true` the client supports the new\n`FoldingRangeRegistrationOptions` return value for the corresponding\nserver capability as well."))
	       ((name . "rangeLimit")
		(type (kind . "base") (name . "uinteger"))
		(optional . t)
		(documentation .
			       "The maximum number of folding ranges that the client prefers to receive\nper document. The value serves as a hint, servers are free to follow the\nlimit."))
	       ((name . "lineFoldingOnly")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "If set, the client signals that it only supports folding complete lines.\nIf set, client will ignore specified `startCharacter` and `endCharacter`\nproperties in a FoldingRange."))
	       ((name . "foldingRangeKind")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "valueSet")
				     (type (kind . "array")
					   (element
					    (kind . "reference")
					    (name . "FoldingRangeKind")))
				     (optional . t)
				     (documentation .
						    "The folding range kind values the client supports. When this\nproperty exists the client also guarantees that it will\nhandle values outside its set gracefully and falls back\nto a default value when unknown."))])))
		(optional . t)
		(documentation .
			       "Specific options for the folding range kind.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "foldingRange")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "collapsedText")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "If set, the client signals that it supports setting collapsedText on\nfolding ranges to display custom labels instead of the default text.\n\n@since 3.17.0")
				     (since . "3.17.0"))])))
		(optional . t)
		(documentation .
			       "Specific options for the folding range.\n\n@since 3.17.0")
		(since . "3.17.0"))]))
 ((name . "SelectionRangeClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration for selection range providers. If this is set to `true`\nthe client supports the new `SelectionRangeRegistrationOptions` return value for the corresponding server\ncapability as well."))]))
 ((name . "PublishDiagnosticsClientCapabilities")
  (properties .
	      [((name . "relatedInformation")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the clients accepts diagnostics with related information."))
	       ((name . "tagSupport")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "valueSet")
				     (type (kind . "array")
					   (element
					    (kind . "reference")
					    (name . "DiagnosticTag")))
				     (documentation .
						    "The tags supported by the client."))])))
		(optional . t)
		(documentation .
			       "Client supports the tag property to provide meta data about a diagnostic.\nClients supporting tags have to handle unknown tags gracefully.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "versionSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client interprets the version property of the\n`textDocument/publishDiagnostics` notification's parameter.\n\n@since 3.15.0")
		(since . "3.15.0"))
	       ((name . "codeDescriptionSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Client supports a codeDescription property\n\n@since 3.16.0")
		(since . "3.16.0"))
	       ((name . "dataSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether code action supports the `data` property which is\npreserved between a `textDocument/publishDiagnostics` and\n`textDocument/codeAction` request.\n\n@since 3.16.0")
		(since . "3.16.0"))])
  (documentation . "The publish diagnostic client capabilities."))
 ((name . "CallHierarchyClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration. If this is set to `true`\nthe client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`\nreturn value for the corresponding server capability as well."))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "SemanticTokensClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration. If this is set to `true`\nthe client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`\nreturn value for the corresponding server capability as well."))
	       ((name . "requests")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "range")
				     (type (kind . "or")
					   (items .
						  [((kind . "base")
						    (name . "boolean"))
						   ((kind . "literal")
						    (value
						     (properties . [])))]))
				     (optional . t)
				     (documentation .
						    "The client will send the `textDocument/semanticTokens/range` request if\nthe server provides a corresponding handler."))
				    ((name . "full")
				     (type (kind . "or")
					   (items .
						  [((kind . "base")
						    (name . "boolean"))
						   ((kind . "literal")
						    (value
						     (properties .
								 [((name
								    .
								    "delta")
								   (type
								    (kind
								     .
								     "base")
								    (name
								     .
								     "boolean"))
								   (optional
								    .
								    t)
								   (documentation
								    .
								    "The client will send the `textDocument/semanticTokens/full/delta` request if\nthe server provides a corresponding handler."))])))]))
				     (optional . t)
				     (documentation .
						    "The client will send the `textDocument/semanticTokens/full` request if\nthe server provides a corresponding handler."))])))
		(documentation .
			       "Which requests the client supports and might send to the server\ndepending on the server's capability. Please note that clients might not\nshow semantic tokens or degrade some of the user experience if a range\nor full request is advertised by the client but not provided by the\nserver. If for example the client capability `requests.full` and\n`request.range` are both set to true but the server only provides a\nrange provider the client might not render a minimap correctly or might\neven decide to not show any semantic tokens at all."))
	       ((name . "tokenTypes")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(documentation .
			       "The token types that the client supports."))
	       ((name . "tokenModifiers")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(documentation .
			       "The token modifiers that the client supports."))
	       ((name . "formats")
		(type (kind . "array")
		      (element (kind . "reference")
			       (name . "TokenFormat")))
		(documentation .
			       "The token formats the clients supports."))
	       ((name . "overlappingTokenSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client supports tokens that can overlap each other."))
	       ((name . "multilineTokenSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client supports tokens that can span multiple lines."))
	       ((name . "serverCancelSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client allows the server to actively cancel a\nsemantic token request, e.g. supports returning\nLSPErrorCodes.ServerCancelled. If a server does the client\nneeds to retrigger the request.\n\n@since 3.17.0")
		(since . "3.17.0"))
	       ((name . "augmentsSyntaxTokens")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the client uses semantic tokens to augment existing\nsyntax tokens. If set to `true` client side created syntax\ntokens and semantic tokens are both used for colorization. If\nset to `false` the client only uses the returned semantic tokens\nfor colorization.\n\nIf the value is `undefined` then the client behavior is not\nspecified.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (documentation . "@since 3.16.0") (since . "3.16.0"))
 ((name . "LinkedEditingRangeClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration. If this is set to `true`\nthe client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`\nreturn value for the corresponding server capability as well."))])
  (documentation .
		 "Client capabilities for the linked editing range request.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "MonikerClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether moniker supports dynamic registration. If this is set to `true`\nthe client supports the new `MonikerRegistrationOptions` return value\nfor the corresponding server capability as well."))])
  (documentation .
		 "Client capabilities specific to the moniker request.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "TypeHierarchyClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration. If this is set to `true`\nthe client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`\nreturn value for the corresponding server capability as well."))])
  (documentation . "@since 3.17.0") (since . "3.17.0"))
 ((name . "InlineValueClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration for inline value providers."))])
  (documentation .
		 "Client capabilities specific to inline values.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "InlayHintClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether inlay hints support dynamic registration."))
	       ((name . "resolveSupport")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name . "properties")
				     (type (kind . "array")
					   (element (kind . "base")
						    (name . "string")))
				     (documentation .
						    "The properties that a client can resolve lazily."))])))
		(optional . t)
		(documentation .
			       "Indicates which properties a client can resolve lazily on an inlay\nhint."))])
  (documentation . "Inlay hint client capabilities.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "DiagnosticClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration. If this is set to `true`\nthe client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`\nreturn value for the corresponding server capability as well."))
	       ((name . "relatedDocumentSupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether the clients supports related documents for document diagnostic pulls."))])
  (documentation .
		 "Client capabilities specific to diagnostic pull requests.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "NotebookDocumentSyncClientCapabilities")
  (properties .
	      [((name . "dynamicRegistration")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "Whether implementation supports dynamic registration. If this is\nset to `true` the client supports the new\n`(TextDocumentRegistrationOptions & StaticRegistrationOptions)`\nreturn value for the corresponding server capability as well."))
	       ((name . "executionSummarySupport")
		(type (kind . "base") (name . "boolean"))
		(optional . t)
		(documentation .
			       "The client supports sending execution summary data per cell."))])
  (documentation .
		 "Notebook specific client capabilities.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((name . "ShowMessageRequestClientCapabilities")
  (properties .
	      [((name . "messageActionItem")
		(type (kind . "literal")
		      (value
		       (properties .
				   [((name .
					   "additionalPropertiesSupport")
				     (type (kind . "base")
					   (name . "boolean"))
				     (optional . t)
				     (documentation .
						    "Whether the client supports additional attributes which\nare preserved and send back to the server in the\nrequest's response."))])))
		(optional . t)
		(documentation .
			       "Capabilities specific to the `MessageActionItem` type."))])
  (documentation . "Show message request client capabilities"))
 ((name . "ShowDocumentClientCapabilities")
  (properties .
	      [((name . "support")
		(type (kind . "base") (name . "boolean"))
		(documentation .
			       "The client has support for the showDocument\nrequest."))])
  (documentation .
		 "Client capabilities for the showDocument request.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "RegularExpressionsClientCapabilities")
  (properties .
	      [((name . "engine")
		(type (kind . "base") (name . "string"))
		(documentation . "The engine's name."))
	       ((name . "version")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation . "The engine's version."))])
  (documentation .
		 "Client capabilities specific to regular expressions.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((name . "MarkdownClientCapabilities")
  (properties .
	      [((name . "parser")
		(type (kind . "base") (name . "string"))
		(documentation . "The name of the parser."))
	       ((name . "version")
		(type (kind . "base") (name . "string"))
		(optional . t)
		(documentation . "The version of the parser."))
	       ((name . "allowedTags")
		(type (kind . "array")
		      (element (kind . "base") (name . "string")))
		(optional . t)
		(documentation .
			       "A list of HTML tags that the client allows / supports in\nMarkdown.\n\n@since 3.17.0")
		(since . "3.17.0"))])
  (documentation .
		 "Client capabilities specific to the used markdown parser.\n\n@since 3.16.0")
  (since . "3.16.0"))]
