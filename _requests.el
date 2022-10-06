;; -*- lisp-data -*-

[((method . "textDocument/implementation")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "Definition"))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DefinitionLink")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "ImplementationParams"))
  (partialResult
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "Location")))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DefinitionLink")))]))
  (registrationOptions
   (kind . "reference")
   (name . "ImplementationRegistrationOptions"))
  (documentation . "A request to resolve the implementation locations of a symbol at a given text\ndocument position. The request's parameter is of type [TextDocumentPositionParams]\n(#TextDocumentPositionParams) the response is of type [Definition](#Definition) or a\nThenable that resolves to such."))
 ((method . "textDocument/typeDefinition")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "Definition"))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DefinitionLink")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "TypeDefinitionParams"))
  (partialResult
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "Location")))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DefinitionLink")))]))
  (registrationOptions
   (kind . "reference")
   (name . "TypeDefinitionRegistrationOptions"))
  (documentation . "A request to resolve the type definition locations of a symbol at a given text\ndocument position. The request's parameter is of type [TextDocumentPositionParams]\n(#TextDocumentPositionParams) the response is of type [Definition](#Definition) or a\nThenable that resolves to such."))
 ((method . "workspace/workspaceFolders")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "WorkspaceFolder")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "serverToClient")
  (documentation . "The `workspace/workspaceFolders` is sent from the server to the client to fetch the open workspace folders."))
 ((method . "workspace/configuration")
  (result
   (kind . "array")
   (element
    (kind . "reference")
    (name . "LSPAny")))
  (messageDirection . "serverToClient")
  (params
   (kind . "and")
   (items .
	  [((kind . "reference")
	    (name . "ConfigurationParams"))
	   ((kind . "reference")
	    (name . "PartialResultParams"))]))
  (documentation . "The 'workspace/configuration' request is sent from the server to the client to fetch a certain\nconfiguration setting.\n\nThis pull model replaces the old push model were the client signaled configuration change via an\nevent. If the server still needs to react to configuration changes (since the server caches the\nresult of `workspace/configuration` requests) the server should register for an empty configuration\nchange event and empty the cache if such an event is received."))
 ((method . "textDocument/documentColor")
  (result
   (kind . "array")
   (element
    (kind . "reference")
    (name . "ColorInformation")))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DocumentColorParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "ColorInformation")))
  (registrationOptions
   (kind . "reference")
   (name . "DocumentColorRegistrationOptions"))
  (documentation . "A request to list all color symbols found in a given text document. The request's\nparameter is of type [DocumentColorParams](#DocumentColorParams) the\nresponse is of type [ColorInformation[]](#ColorInformation) or a Thenable\nthat resolves to such."))
 ((method . "textDocument/colorPresentation")
  (result
   (kind . "array")
   (element
    (kind . "reference")
    (name . "ColorPresentation")))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "ColorPresentationParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "ColorPresentation")))
  (registrationOptions
   (kind . "and")
   (items .
	  [((kind . "reference")
	    (name . "WorkDoneProgressOptions"))
	   ((kind . "reference")
	    (name . "TextDocumentRegistrationOptions"))]))
  (documentation . "A request to list all presentation for a color. The request's\nparameter is of type [ColorPresentationParams](#ColorPresentationParams) the\nresponse is of type [ColorInformation[]](#ColorInformation) or a Thenable\nthat resolves to such."))
 ((method . "textDocument/foldingRange")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "FoldingRange")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "FoldingRangeParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "FoldingRange")))
  (registrationOptions
   (kind . "reference")
   (name . "FoldingRangeRegistrationOptions"))
  (documentation . "A request to provide folding ranges in a document. The request's\nparameter is of type [FoldingRangeParams](#FoldingRangeParams), the\nresponse is of type [FoldingRangeList](#FoldingRangeList) or a Thenable\nthat resolves to such."))
 ((method . "textDocument/declaration")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "Declaration"))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DeclarationLink")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DeclarationParams"))
  (partialResult
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "Location")))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DeclarationLink")))]))
  (registrationOptions
   (kind . "reference")
   (name . "DeclarationRegistrationOptions"))
  (documentation . "A request to resolve the type definition locations of a symbol at a given text\ndocument position. The request's parameter is of type [TextDocumentPositionParams]\n(#TextDocumentPositionParams) the response is of type [Declaration](#Declaration)\nor a typed array of [DeclarationLink](#DeclarationLink) or a Thenable that resolves\nto such."))
 ((method . "textDocument/selectionRange")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "SelectionRange")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "SelectionRangeParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "SelectionRange")))
  (registrationOptions
   (kind . "reference")
   (name . "SelectionRangeRegistrationOptions"))
  (documentation . "A request to provide selection ranges in a document. The request's\nparameter is of type [SelectionRangeParams](#SelectionRangeParams), the\nresponse is of type [SelectionRange[]](#SelectionRange[]) or a Thenable\nthat resolves to such."))
 ((method . "window/workDoneProgress/create")
  (result
   (kind . "base")
   (name . "null"))
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "WorkDoneProgressCreateParams"))
  (documentation . "The `window/workDoneProgress/create` request is sent from the server to the client to initiate progress\nreporting from the server."))
 ((method . "textDocument/prepareCallHierarchy")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "CallHierarchyItem")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CallHierarchyPrepareParams"))
  (registrationOptions
   (kind . "reference")
   (name . "CallHierarchyRegistrationOptions"))
  (documentation . "A request to result a `CallHierarchyItem` in a document at a given position.\nCan be used as an input to an incoming or outgoing call hierarchy.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "callHierarchy/incomingCalls")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "CallHierarchyIncomingCall")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CallHierarchyIncomingCallsParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "CallHierarchyIncomingCall")))
  (documentation . "A request to resolve the incoming calls for a given `CallHierarchyItem`.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "callHierarchy/outgoingCalls")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "CallHierarchyOutgoingCall")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CallHierarchyOutgoingCallsParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "CallHierarchyOutgoingCall")))
  (documentation . "A request to resolve the outgoing calls for a given `CallHierarchyItem`.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "textDocument/semanticTokens/full")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "SemanticTokens"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "SemanticTokensParams"))
  (partialResult
   (kind . "reference")
   (name . "SemanticTokensPartialResult"))
  (registrationMethod . "textDocument/semanticTokens")
  (registrationOptions
   (kind . "reference")
   (name . "SemanticTokensRegistrationOptions"))
  (documentation . "@since 3.16.0")
  (since . "3.16.0"))
 ((method . "textDocument/semanticTokens/full/delta")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "SemanticTokens"))
	   ((kind . "reference")
	    (name . "SemanticTokensDelta"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "SemanticTokensDeltaParams"))
  (partialResult
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "SemanticTokensPartialResult"))
	   ((kind . "reference")
	    (name . "SemanticTokensDeltaPartialResult"))]))
  (registrationMethod . "textDocument/semanticTokens")
  (registrationOptions
   (kind . "reference")
   (name . "SemanticTokensRegistrationOptions"))
  (documentation . "@since 3.16.0")
  (since . "3.16.0"))
 ((method . "textDocument/semanticTokens/range")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "SemanticTokens"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "SemanticTokensRangeParams"))
  (partialResult
   (kind . "reference")
   (name . "SemanticTokensPartialResult"))
  (registrationMethod . "textDocument/semanticTokens")
  (documentation . "@since 3.16.0")
  (since . "3.16.0"))
 ((method . "workspace/semanticTokens/refresh")
  (result
   (kind . "base")
   (name . "null"))
  (messageDirection . "clientToServer")
  (documentation . "@since 3.16.0")
  (since . "3.16.0"))
 ((method . "window/showDocument")
  (result
   (kind . "reference")
   (name . "ShowDocumentResult"))
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "ShowDocumentParams"))
  (documentation . "A request to show a document. This request might open an\nexternal program depending on the value of the URI to open.\nFor example a request to open `https://code.visualstudio.com/`\nwill very likely open the URI in a WEB browser.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "textDocument/linkedEditingRange")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "LinkedEditingRanges"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "LinkedEditingRangeParams"))
  (registrationOptions
   (kind . "reference")
   (name . "LinkedEditingRangeRegistrationOptions"))
  (documentation . "A request to provide ranges that can be edited together.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "workspace/willCreateFiles")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "WorkspaceEdit"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CreateFilesParams"))
  (registrationOptions
   (kind . "reference")
   (name . "FileOperationRegistrationOptions"))
  (documentation . "The will create files request is sent from the client to the server before files are actually\ncreated as long as the creation is triggered from within the client.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "workspace/willRenameFiles")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "WorkspaceEdit"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "RenameFilesParams"))
  (registrationOptions
   (kind . "reference")
   (name . "FileOperationRegistrationOptions"))
  (documentation . "The will rename files request is sent from the client to the server before files are actually\nrenamed as long as the rename is triggered from within the client.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "workspace/willDeleteFiles")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "WorkspaceEdit"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DeleteFilesParams"))
  (registrationOptions
   (kind . "reference")
   (name . "FileOperationRegistrationOptions"))
  (documentation . "The did delete files notification is sent from the client to the server when\nfiles were deleted from within the client.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "textDocument/moniker")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "Moniker")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "MonikerParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "Moniker")))
  (registrationOptions
   (kind . "reference")
   (name . "MonikerRegistrationOptions"))
  (documentation . "A request to get the moniker of a symbol at a given text document position.\nThe request parameter is of type [TextDocumentPositionParams](#TextDocumentPositionParams).\nThe response is of type [Moniker[]](#Moniker[]) or `null`."))
 ((method . "textDocument/prepareTypeHierarchy")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "TypeHierarchyItem")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "TypeHierarchyPrepareParams"))
  (registrationOptions
   (kind . "reference")
   (name . "TypeHierarchyRegistrationOptions"))
  (documentation . "A request to result a `TypeHierarchyItem` in a document at a given position.\nCan be used as an input to a subtypes or supertypes type hierarchy.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "typeHierarchy/supertypes")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "TypeHierarchyItem")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "TypeHierarchySupertypesParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "TypeHierarchyItem")))
  (documentation . "A request to resolve the supertypes for a given `TypeHierarchyItem`.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "typeHierarchy/subtypes")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "TypeHierarchyItem")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "TypeHierarchySubtypesParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "TypeHierarchyItem")))
  (documentation . "A request to resolve the subtypes for a given `TypeHierarchyItem`.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "textDocument/inlineValue")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "InlineValue")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "InlineValueParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "InlineValue")))
  (registrationOptions
   (kind . "reference")
   (name . "InlineValueRegistrationOptions"))
  (documentation . "A request to provide inline values in a document. The request's parameter is of\ntype [InlineValueParams](#InlineValueParams), the response is of type\n[InlineValue[]](#InlineValue[]) or a Thenable that resolves to such.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "workspace/inlineValue/refresh")
  (result
   (kind . "base")
   (name . "null"))
  (messageDirection . "clientToServer")
  (documentation . "@since 3.17.0")
  (since . "3.17.0"))
 ((method . "textDocument/inlayHint")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "InlayHint")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "InlayHintParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "InlayHint")))
  (registrationOptions
   (kind . "reference")
   (name . "InlayHintRegistrationOptions"))
  (documentation . "A request to provide inlay hints in a document. The request's parameter is of\ntype [InlayHintsParams](#InlayHintsParams), the response is of type\n[InlayHint[]](#InlayHint[]) or a Thenable that resolves to such.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "inlayHint/resolve")
  (result
   (kind . "reference")
   (name . "InlayHint"))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "InlayHint"))
  (documentation . "A request to resolve additional properties for an inlay hint.\nThe request's parameter is of type [InlayHint](#InlayHint), the response is\nof type [InlayHint](#InlayHint) or a Thenable that resolves to such.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "workspace/inlayHint/refresh")
  (result
   (kind . "base")
   (name . "null"))
  (messageDirection . "clientToServer")
  (documentation . "@since 3.17.0")
  (since . "3.17.0"))
 ((method . "textDocument/diagnostic")
  (result
   (kind . "reference")
   (name . "DocumentDiagnosticReport"))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DocumentDiagnosticParams"))
  (partialResult
   (kind . "reference")
   (name . "DocumentDiagnosticReportPartialResult"))
  (errorData
   (kind . "reference")
   (name . "DiagnosticServerCancellationData"))
  (registrationOptions
   (kind . "reference")
   (name . "DiagnosticRegistrationOptions"))
  (documentation . "The document diagnostic request definition.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "workspace/diagnostic")
  (result
   (kind . "reference")
   (name . "WorkspaceDiagnosticReport"))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "WorkspaceDiagnosticParams"))
  (partialResult
   (kind . "reference")
   (name . "WorkspaceDiagnosticReportPartialResult"))
  (errorData
   (kind . "reference")
   (name . "DiagnosticServerCancellationData"))
  (documentation . "The workspace diagnostic request definition.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "workspace/diagnostic/refresh")
  (result
   (kind . "base")
   (name . "null"))
  (messageDirection . "clientToServer")
  (documentation . "The diagnostic refresh request definition.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "client/registerCapability")
  (result
   (kind . "base")
   (name . "null"))
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "RegistrationParams"))
  (documentation . "The `client/registerCapability` request is sent from the server to the client to register a new capability\nhandler on the client side."))
 ((method . "client/unregisterCapability")
  (result
   (kind . "base")
   (name . "null"))
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "UnregistrationParams"))
  (documentation . "The `client/unregisterCapability` request is sent from the server to the client to unregister a previously registered capability\nhandler on the client side."))
 ((method . "initialize")
  (result
   (kind . "reference")
   (name . "InitializeResult"))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "InitializeParams"))
  (errorData
   (kind . "reference")
   (name . "InitializeError"))
  (documentation . "The initialize request is sent from the client to the server.\nIt is sent once as the request after starting up the server.\nThe requests parameter is of type [InitializeParams](#InitializeParams)\nthe response if of type [InitializeResult](#InitializeResult) of a Thenable that\nresolves to such."))
 ((method . "shutdown")
  (result
   (kind . "base")
   (name . "null"))
  (messageDirection . "clientToServer")
  (documentation . "A shutdown request is sent from the client to the server.\nIt is sent once when the client decides to shutdown the\nserver. The only notification that is sent after a shutdown request\nis the exit event."))
 ((method . "window/showMessageRequest")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "MessageActionItem"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "ShowMessageRequestParams"))
  (documentation . "The show message request is sent from the server to the client to show a message\nand a set of options actions to the user."))
 ((method . "textDocument/willSaveWaitUntil")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "TextEdit")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "WillSaveTextDocumentParams"))
  (registrationOptions
   (kind . "reference")
   (name . "TextDocumentRegistrationOptions"))
  (documentation . "A document will save request is sent from the client to the server before\nthe document is actually saved. The request can return an array of TextEdits\nwhich will be applied to the text document before it is saved. Please note that\nclients might drop results if computing the text edits took too long or if a\nserver constantly fails on this request. This is done to keep the save fast and\nreliable."))
 ((method . "textDocument/completion")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "CompletionItem")))
	   ((kind . "reference")
	    (name . "CompletionList"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CompletionParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "CompletionItem")))
  (registrationOptions
   (kind . "reference")
   (name . "CompletionRegistrationOptions"))
  (documentation . "Request to request completion at a given text document position. The request's\nparameter is of type [TextDocumentPosition](#TextDocumentPosition) the response\nis of type [CompletionItem[]](#CompletionItem) or [CompletionList](#CompletionList)\nor a Thenable that resolves to such.\n\nThe request can delay the computation of the [`detail`](#CompletionItem.detail)\nand [`documentation`](#CompletionItem.documentation) properties to the `completionItem/resolve`\nrequest. However, properties that are needed for the initial sorting and filtering, like `sortText`,\n`filterText`, `insertText`, and `textEdit`, must not be changed during resolve."))
 ((method . "completionItem/resolve")
  (result
   (kind . "reference")
   (name . "CompletionItem"))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CompletionItem"))
  (documentation . "Request to resolve additional information for a given completion item.The request's\nparameter is of type [CompletionItem](#CompletionItem) the response\nis of type [CompletionItem](#CompletionItem) or a Thenable that resolves to such."))
 ((method . "textDocument/hover")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "Hover"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "HoverParams"))
  (registrationOptions
   (kind . "reference")
   (name . "HoverRegistrationOptions"))
  (documentation . "Request to request hover information at a given text document position. The request's\nparameter is of type [TextDocumentPosition](#TextDocumentPosition) the response is of\ntype [Hover](#Hover) or a Thenable that resolves to such."))
 ((method . "textDocument/signatureHelp")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "SignatureHelp"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "SignatureHelpParams"))
  (registrationOptions
   (kind . "reference")
   (name . "SignatureHelpRegistrationOptions")))
 ((method . "textDocument/definition")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "Definition"))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DefinitionLink")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DefinitionParams"))
  (partialResult
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "Location")))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DefinitionLink")))]))
  (registrationOptions
   (kind . "reference")
   (name . "DefinitionRegistrationOptions"))
  (documentation . "A request to resolve the definition location of a symbol at a given text\ndocument position. The request's parameter is of type [TextDocumentPosition]\n(#TextDocumentPosition) the response is of either type [Definition](#Definition)\nor a typed array of [DefinitionLink](#DefinitionLink) or a Thenable that resolves\nto such."))
 ((method . "textDocument/references")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "Location")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "ReferenceParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "Location")))
  (registrationOptions
   (kind . "reference")
   (name . "ReferenceRegistrationOptions"))
  (documentation . "A request to resolve project-wide references for the symbol denoted\nby the given text document position. The request's parameter is of\ntype [ReferenceParams](#ReferenceParams) the response is of type\n[Location[]](#Location) or a Thenable that resolves to such."))
 ((method . "textDocument/documentHighlight")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DocumentHighlight")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DocumentHighlightParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "DocumentHighlight")))
  (registrationOptions
   (kind . "reference")
   (name . "DocumentHighlightRegistrationOptions"))
  (documentation . "Request to resolve a [DocumentHighlight](#DocumentHighlight) for a given\ntext document position. The request's parameter is of type [TextDocumentPosition]\n(#TextDocumentPosition) the request response is of type [DocumentHighlight[]]\n(#DocumentHighlight) or a Thenable that resolves to such."))
 ((method . "textDocument/documentSymbol")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "SymbolInformation")))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DocumentSymbol")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DocumentSymbolParams"))
  (partialResult
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "SymbolInformation")))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DocumentSymbol")))]))
  (registrationOptions
   (kind . "reference")
   (name . "DocumentSymbolRegistrationOptions"))
  (documentation . "A request to list all symbols found in a given text document. The request's\nparameter is of type [TextDocumentIdentifier](#TextDocumentIdentifier) the\nresponse is of type [SymbolInformation[]](#SymbolInformation) or a Thenable\nthat resolves to such."))
 ((method . "textDocument/codeAction")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "or")
	     (items .
		    [((kind . "reference")
		      (name . "Command"))
		     ((kind . "reference")
		      (name . "CodeAction"))])))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CodeActionParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "or")
    (items .
	   [((kind . "reference")
	     (name . "Command"))
	    ((kind . "reference")
	     (name . "CodeAction"))])))
  (registrationOptions
   (kind . "reference")
   (name . "CodeActionRegistrationOptions"))
  (documentation . "A request to provide commands for the given text document and range."))
 ((method . "codeAction/resolve")
  (result
   (kind . "reference")
   (name . "CodeAction"))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CodeAction"))
  (documentation . "Request to resolve additional information for a given code action.The request's\nparameter is of type [CodeAction](#CodeAction) the response\nis of type [CodeAction](#CodeAction) or a Thenable that resolves to such."))
 ((method . "workspace/symbol")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "SymbolInformation")))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "WorkspaceSymbol")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "WorkspaceSymbolParams"))
  (partialResult
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "SymbolInformation")))
	   ((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "WorkspaceSymbol")))]))
  (registrationOptions
   (kind . "reference")
   (name . "WorkspaceSymbolRegistrationOptions"))
  (documentation . "A request to list project-wide symbols matching the query string given\nby the [WorkspaceSymbolParams](#WorkspaceSymbolParams). The response is\nof type [SymbolInformation[]](#SymbolInformation) or a Thenable that\nresolves to such.\n\n@since 3.17.0 - support for WorkspaceSymbol in the returned data. Clients\n need to advertise support for WorkspaceSymbols via the client capability\n `workspace.symbol.resolveSupport`.\n")
  (since . "3.17.0 - support for WorkspaceSymbol in the returned data. Clients\nneed to advertise support for WorkspaceSymbols via the client capability\n`workspace.symbol.resolveSupport`."))
 ((method . "workspaceSymbol/resolve")
  (result
   (kind . "reference")
   (name . "WorkspaceSymbol"))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "WorkspaceSymbol"))
  (documentation . "A request to resolve the range inside the workspace\nsymbol's location.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "textDocument/codeLens")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "CodeLens")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CodeLensParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "CodeLens")))
  (registrationOptions
   (kind . "reference")
   (name . "CodeLensRegistrationOptions"))
  (documentation . "A request to provide code lens for the given text document."))
 ((method . "codeLens/resolve")
  (result
   (kind . "reference")
   (name . "CodeLens"))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CodeLens"))
  (documentation . "A request to resolve a command for a given code lens."))
 ((method . "workspace/codeLens/refresh")
  (result
   (kind . "base")
   (name . "null"))
  (messageDirection . "serverToClient")
  (documentation . "A request to refresh all code actions\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "textDocument/documentLink")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "DocumentLink")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DocumentLinkParams"))
  (partialResult
   (kind . "array")
   (element
    (kind . "reference")
    (name . "DocumentLink")))
  (registrationOptions
   (kind . "reference")
   (name . "DocumentLinkRegistrationOptions"))
  (documentation . "A request to provide document links"))
 ((method . "documentLink/resolve")
  (result
   (kind . "reference")
   (name . "DocumentLink"))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DocumentLink"))
  (documentation . "Request to resolve additional information for a given document link. The request's\nparameter is of type [DocumentLink](#DocumentLink) the response\nis of type [DocumentLink](#DocumentLink) or a Thenable that resolves to such."))
 ((method . "textDocument/formatting")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "TextEdit")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DocumentFormattingParams"))
  (registrationOptions
   (kind . "reference")
   (name . "DocumentFormattingRegistrationOptions"))
  (documentation . "A request to to format a whole document."))
 ((method . "textDocument/rangeFormatting")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "TextEdit")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DocumentRangeFormattingParams"))
  (registrationOptions
   (kind . "reference")
   (name . "DocumentRangeFormattingRegistrationOptions"))
  (documentation . "A request to to format a range in a document."))
 ((method . "textDocument/onTypeFormatting")
  (result
   (kind . "or")
   (items .
	  [((kind . "array")
	    (element
	     (kind . "reference")
	     (name . "TextEdit")))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DocumentOnTypeFormattingParams"))
  (registrationOptions
   (kind . "reference")
   (name . "DocumentOnTypeFormattingRegistrationOptions"))
  (documentation . "A request to format a document on type."))
 ((method . "textDocument/rename")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "WorkspaceEdit"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "RenameParams"))
  (registrationOptions
   (kind . "reference")
   (name . "RenameRegistrationOptions"))
  (documentation . "A request to rename a symbol."))
 ((method . "textDocument/prepareRename")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "PrepareRenameResult"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "PrepareRenameParams"))
  (documentation . "A request to test and perform the setup necessary for a rename.\n\n@since 3.16 - support for default behavior")
  (since . "3.16 - support for default behavior"))
 ((method . "workspace/executeCommand")
  (result
   (kind . "or")
   (items .
	  [((kind . "reference")
	    (name . "LSPAny"))
	   ((kind . "base")
	    (name . "null"))]))
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "ExecuteCommandParams"))
  (registrationOptions
   (kind . "reference")
   (name . "ExecuteCommandRegistrationOptions"))
  (documentation . "A request send from the client to the server to execute a command. The request might return\na workspace edit which the client will apply to the workspace."))
 ((method . "workspace/applyEdit")
  (result
   (kind . "reference")
   (name . "ApplyWorkspaceEditResult"))
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "ApplyWorkspaceEditParams"))
  (documentation . "A request sent from the server to the client to modified certain resources."))]
