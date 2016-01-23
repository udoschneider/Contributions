"Filed out from Dolphin Smalltalk 7"!

ValuePresenter subclass: #MarkdownPresenter
	instanceVariableNames: 'converter'
	classVariableNames: ''
	poolDictionaries: 'SHDocVwConstants'
	classInstanceVariableNames: ''!
MarkdownPresenter guid: (GUID fromString: '{C4A4F15A-0780-418A-B5ED-04440A0B82DA}')!
MarkdownPresenter comment: ''!
!MarkdownPresenter categoriesForClass!MVP-Presenters! !
!MarkdownPresenter methodsFor!

caretPosition: anIntegerOrNil 
	converter caretPosition: anIntegerOrNil !

initialize
	super initialize.
	converter := MarkdownToHtmlCssConverter new!

onValueChanged

	super onValueChanged.
	self updatePreview!

onViewOpened
super onViewOpened.
self updatePreview!

updatePreview
	| browser html document element |
	browser := self view controlDispatch.
	browser value: 'about:blank'.
	[browser readyState == READYSTATE_COMPLETE] whileFalse: [SessionManager inputState pumpMessages].
	html := converter leftToRight: model value.
	document := browser document.
	document write: html.
	element := document getElementById: 'cursor'.
	element isNull ifFalse: [element scrollIntoView: true]! !
!MarkdownPresenter categoriesFor: #caretPosition:!accessing!public! !
!MarkdownPresenter categoriesFor: #initialize!public! !
!MarkdownPresenter categoriesFor: #onValueChanged!event handling!public! !
!MarkdownPresenter categoriesFor: #onViewOpened!public! !
!MarkdownPresenter categoriesFor: #updatePreview!event handling!private! !

!MarkdownPresenter class methodsFor!

defaultModel
	"Answer a default model to be assigned to the receiver when it is initialized."

	^'' asValue!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.AXControlSite)  98 25 0 0 98 2 8 1140916224 1 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 0 0 0 416 0 8045337 8 'Shell.Explorer' 787462 ##(Smalltalk.IWebBrowser2)  0 0 1444166 1 ##(Smalltalk.TKindInterfaceAnalyzer)  590598 ##(Smalltalk.ITypeInfo)  0 0 525062 ##(Smalltalk.TYPEATTR)  0 640 608 0 1378630 2 ##(Smalltalk.AXTypeLibraryAnalyzer)  590342 ##(Smalltalk.ITypeLib2)  0 0 257 524550 ##(Smalltalk.TLIBATTR)  8 #[192 42 178 234 193 48 207 17 167 235 0 0 192 91 174 11 0 0 0 0 1 0 0 0 1 0 1 0 8 0 0 0] 8 '' 8 'Internet Explorer' 8 #SHDocVwLib 8 #SHDocVwConstants 234 240 98 2 8 #GUID 896 0 0 0 11 8 'IWebBrowser2' 0 0 0 918022 ##(Smalltalk.IDolphinAxHost)  0 0 722438 ##(Smalltalk.AXEventSink)  234 240 98 76 213 8 #DownloadBegin 529 8 #WindowSetLeft: 217 8 #ProgressChange:ProgressMax: 531 8 #WindowSetTop: 533 8 #WindowSetWidth: 535 8 #WindowSetHeight: 225 8 #PropertyChange: 227 8 #TitleChange: 527 8 #WindowClosing:Cancel: 537 8 #ClientToHostWindow:CY: 539 8 #SetSecureLockIcon: 541 8 #FileDownload:Cancel: 543 8 #NavigateError:URL:Frame:StatusCode:Cancel: 451 8 #PrintTemplateInstantiation: 453 8 #PrintTemplateTeardown: 455 8 #UpdatePageStatus:nPage:fDone: 545 8 #PrivacyImpactedStateChange: 547 8 #NewWindow3:Cancel:dwFlags:bstrUrlContext:bstrUrl: 565 8 #SetPhishingFilterStatus: 567 8 #WindowStateChanged:dwValidFlagsMask: 569 8 #NewProcess:pWB2:Cancel: 571 8 #ThirdPartyUrlBlocked:dwCount: 573 8 #RedirectXDomainBlocked:StartURL:RedirectURL:Frame:StatusCode: 501 8 #BeforeNavigate2:URL:Flags:TargetFrameName:PostData:Headers:Cancel: 503 8 #NewWindow2:Cancel: 505 8 #NavigateComplete2:URL: 507 8 #OnQuit 509 8 #OnVisible: 511 8 #OnToolBar: 513 8 #OnMenuBar: 515 8 #OnStatusBar: 517 8 #OnFullScreen: 519 8 #DocumentComplete:URL: 521 8 #OnTheaterMode: 205 8 #StatusTextChange: 525 8 #WindowSetResizable: 209 8 #DownloadComplete 211 8 #CommandStateChange:Enable: 416 1049094 ##(Smalltalk.IConnectionPoint)  0 0 3 1378630 1 ##(Smalltalk.TKindDispatchAnalyzer)  626 0 0 658 0 1696 1680 0 704 0 262198 ##(Smalltalk.GUID)  16 160 21 167 52 135 101 208 17 146 74 0 32 175 199 172 77 25 0 0 0 1 2763 234 240 98 0 1508358 ##(Smalltalk.IAxWinAmbientDispatchEx)  0 0 0 0 524806 ##(Smalltalk.IUnknown)  0 0 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  5887 21 1986 1179 289 416 1922 8 #restoreAmbientProperties 1776 416 1922 8 #controlBinaryStoreBytes: 98 1 8 #[97 249 86 136 10 52 208 17 169 107 0 192 79 215 5 162 76 0 0 0 224 60 0 0 226 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 76 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 224 208 87 0 115 53 207 17 174 105 8 0 43 46 18 98 8 0 0 0 0 0 0 0 76 0 0 0 1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70 128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 11 0 0 10 0 0 0 204 13 0 0 154 0 0 0] 98 0 1986 193 193 0 27 )! !
!MarkdownPresenter class categoriesFor: #defaultModel!models!public! !
!MarkdownPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

