;; -*- lisp-data -*-

[((method . "workspace/didChangeWorkspaceFolders")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidChangeWorkspaceFoldersParams"))
  (documentation . "The `workspace/didChangeWorkspaceFolders` notification is sent from the client to the server when the workspace\nfolder configuration changes."))
 ((method . "window/workDoneProgress/cancel")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "WorkDoneProgressCancelParams"))
  (documentation . "The `window/workDoneProgress/cancel` notification is sent from  the client to the server to cancel a progress\ninitiated on the server side."))
 ((method . "workspace/didCreateFiles")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "CreateFilesParams"))
  (registrationOptions
   (kind . "reference")
   (name . "FileOperationRegistrationOptions"))
  (documentation . "The did create files notification is sent from the client to the server when\nfiles were created from within the client.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "workspace/didRenameFiles")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "RenameFilesParams"))
  (registrationOptions
   (kind . "reference")
   (name . "FileOperationRegistrationOptions"))
  (documentation . "The did rename files notification is sent from the client to the server when\nfiles were renamed from within the client.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "workspace/didDeleteFiles")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DeleteFilesParams"))
  (registrationOptions
   (kind . "reference")
   (name . "FileOperationRegistrationOptions"))
  (documentation . "The will delete files request is sent from the client to the server before files are actually\ndeleted as long as the deletion is triggered from within the client.\n\n@since 3.16.0")
  (since . "3.16.0"))
 ((method . "notebookDocument/didOpen")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidOpenNotebookDocumentParams"))
  (registrationMethod . "notebookDocument/sync")
  (documentation . "A notification sent when a notebook opens.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "notebookDocument/didChange")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidChangeNotebookDocumentParams"))
  (registrationMethod . "notebookDocument/sync"))
 ((method . "notebookDocument/didSave")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidSaveNotebookDocumentParams"))
  (registrationMethod . "notebookDocument/sync")
  (documentation . "A notification sent when a notebook document is saved.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "notebookDocument/didClose")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidCloseNotebookDocumentParams"))
  (registrationMethod . "notebookDocument/sync")
  (documentation . "A notification sent when a notebook closes.\n\n@since 3.17.0")
  (since . "3.17.0"))
 ((method . "initialized")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "InitializedParams"))
  (documentation . "The initialized notification is sent from the client to the\nserver after the client is fully initialized and the server\nis allowed to send requests from the server to the client."))
 ((method . "exit")
  (messageDirection . "clientToServer")
  (documentation . "The exit event is sent from the client to the server to\nask the server to exit its process."))
 ((method . "workspace/didChangeConfiguration")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidChangeConfigurationParams"))
  (registrationOptions
   (kind . "reference")
   (name . "DidChangeConfigurationRegistrationOptions"))
  (documentation . "The configuration change notification is sent from the client to the server\nwhen the client's configuration has changed. The notification contains\nthe changed configuration as defined by the language client."))
 ((method . "window/showMessage")
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "ShowMessageParams"))
  (documentation . "The show message notification is sent from a server to a client to ask\nthe client to display a particular message in the user interface."))
 ((method . "window/logMessage")
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "LogMessageParams"))
  (documentation . "The log message notification is sent from the server to the client to ask\nthe client to log a particular message."))
 ((method . "telemetry/event")
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "LSPAny"))
  (documentation . "The telemetry event notification is sent from the server to the client to ask\nthe client to log telemetry data."))
 ((method . "textDocument/didOpen")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidOpenTextDocumentParams"))
  (registrationOptions
   (kind . "reference")
   (name . "TextDocumentRegistrationOptions"))
  (documentation . "The document open notification is sent from the client to the server to signal\nnewly opened text documents. The document's truth is now managed by the client\nand the server must not try to read the document's truth using the document's\nuri. Open in this sense means it is managed by the client. It doesn't necessarily\nmean that its content is presented in an editor. An open notification must not\nbe sent more than once without a corresponding close notification send before.\nThis means open and close notification must be balanced and the max open count\nis one."))
 ((method . "textDocument/didChange")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidChangeTextDocumentParams"))
  (registrationOptions
   (kind . "reference")
   (name . "TextDocumentChangeRegistrationOptions"))
  (documentation . "The document change notification is sent from the client to the server to signal\nchanges to a text document."))
 ((method . "textDocument/didClose")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidCloseTextDocumentParams"))
  (registrationOptions
   (kind . "reference")
   (name . "TextDocumentRegistrationOptions"))
  (documentation . "The document close notification is sent from the client to the server when\nthe document got closed in the client. The document's truth now exists where\nthe document's uri points to (e.g. if the document's uri is a file uri the\ntruth now exists on disk). As with the open notification the close notification\nis about managing the document's content. Receiving a close notification\ndoesn't mean that the document was open in an editor before. A close\nnotification requires a previous open notification to be sent."))
 ((method . "textDocument/didSave")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidSaveTextDocumentParams"))
  (registrationOptions
   (kind . "reference")
   (name . "TextDocumentSaveRegistrationOptions"))
  (documentation . "The document save notification is sent from the client to the server when\nthe document got saved in the client."))
 ((method . "textDocument/willSave")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "WillSaveTextDocumentParams"))
  (registrationOptions
   (kind . "reference")
   (name . "TextDocumentRegistrationOptions"))
  (documentation . "A document will save notification is sent from the client to the server before\nthe document is actually saved."))
 ((method . "workspace/didChangeWatchedFiles")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "DidChangeWatchedFilesParams"))
  (registrationOptions
   (kind . "reference")
   (name . "DidChangeWatchedFilesRegistrationOptions"))
  (documentation . "The watched files notification is sent from the client to the server when\nthe client detects changes to file watched by the language client."))
 ((method . "textDocument/publishDiagnostics")
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "PublishDiagnosticsParams"))
  (documentation . "Diagnostics notification are sent from the server to the client to signal\nresults of validation runs."))
 ((method . "$/setTrace")
  (messageDirection . "clientToServer")
  (params
   (kind . "reference")
   (name . "SetTraceParams")))
 ((method . "$/logTrace")
  (messageDirection . "serverToClient")
  (params
   (kind . "reference")
   (name . "LogTraceParams")))
 ((method . "$/cancelRequest")
  (messageDirection . "both")
  (params
   (kind . "reference")
   (name . "CancelParams")))
 ((method . "$/progress")
  (messageDirection . "both")
  (params
   (kind . "reference")
   (name . "ProgressParams")))]
