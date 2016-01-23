"Filed out from Dolphin Smalltalk 7"!

ValuePresenter subclass: #MarkdownPreviewPresenter
	instanceVariableNames: 'sourcePresenter previewPresenter markdownPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MarkdownPreviewPresenter guid: (GUID fromString: '{8C5D7AD6-6B9D-47FF-8FAB-A7EA7DC0A294}')!
MarkdownPreviewPresenter comment: ''!
!MarkdownPreviewPresenter categoriesForClass!MVP-Presenters! !
!MarkdownPreviewPresenter methodsFor!

createComponents
	super createComponents.
	sourcePresenter := self add: TextPresenter new name: 'source'.
	previewPresenter := self add: MarkdownPresenter new name: 'preview'.
	markdownPresenter := self add: TextPresenter new name: 'markdown'.!

createSchematicWiring
	super createSchematicWiring.
	self 
		when: #valueChanged
		send: #onCaretPositionChanged
		to: self.
sourcePresenter when: #keyTyped: send: #onCaretPositionChanged to: self!

model: aModel 
	super model: aModel.
	sourcePresenter model: self model.
	previewPresenter model: self model.
markdownPresenter model: self model!

onCaretPositionChanged
	| caretPosition |
	caretPosition := sourcePresenter view caretPosition.
	previewPresenter caretPosition: caretPosition.
	markdownPresenter view typeconverter caretPosition: caretPosition!

onViewOpened
	super onViewOpened.
	(sourcePresenter view)
		lexer: #markdown;
		textStyles: self class markdownTextStyles! !
!MarkdownPreviewPresenter categoriesFor: #createComponents!public! !
!MarkdownPreviewPresenter categoriesFor: #createSchematicWiring!event handling!public! !
!MarkdownPreviewPresenter categoriesFor: #model:!public! !
!MarkdownPreviewPresenter categoriesFor: #onCaretPositionChanged!event handling!public! !
!MarkdownPreviewPresenter categoriesFor: #onViewOpened!public! !

!MarkdownPreviewPresenter class methodsFor!

defaultModel
	"Answer a default model to be assigned to the receiver when it is initialized."

	^'' asValue!

markdownTextStyles
	| answer |
	answer := Set new.
	answer
		add: ((ScintillaTextStyle name: 0)
					name: #SCE_MARKDOWN_DEFAULT;
					description: 'Normal';
					backcolor: Color white;
					yourself);
		add: ((ScintillaTextStyle name: 1)
					name: #SCE_MARKDOWN_LINE_BEGIN;
					description: 'Special, e.g. end-of-line codes if enabled';
					yourself);
		add: ((ScintillaTextStyle name: 2)
					name: #SCE_MARKDOWN_STRONG1;
					description: 'STRONG **double asterisk**';
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: 3)
					name: #SCE_MARKDOWN_STRONG2;
					description: 'STRONG 2 (not used)';
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: 4)
					name: #SCE_MARKDOWN_EM1;
					description: 'EM1 *single asterisk*';
					isItalic: true;
					yourself);
		add: ((ScintillaTextStyle name: 5)
					name: #SCE_MARKDOWN_EM2;
					description: 'EM2 _single underline_';
					isUnderlined: true;
					yourself);
		add: ((ScintillaTextStyle name: 6)
					name: #SCE_MARKDOWN_HEADER1;
					forecolor: (Color fromHTMLSpec: '#FF7700');
					description: 'H1';
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: 7)
					name: #SCE_MARKDOWN_HEADER2;
					forecolor: (Color fromHTMLSpec: '#DD6600');
					description: 'H2';
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: 8)
					name: #SCE_MARKDOWN_HEADER3;
					forecolor: (Color fromHTMLSpec: '#BB5500');
					description: 'H3';
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: 9)
					name: #SCE_MARKDOWN_HEADER4;
					description: 'H4';
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: 10)
					name: #SCE_MARKDOWN_HEADER5;
					description: 'H5';
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: 11)
					name: #SCE_MARKDOWN_HEADER6;
					description: 'H6';
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: 12)
					name: #SCE_MARKDOWN_PRECHAR;
					description: 'prechar (up to three indent spaces, eg for a second-level list)';
					forecolor: (Color fromHTMLSpec: '#000000');
					backcolor: (Color fromHTMLSpec: '#EEEEAA');
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: 13)
					name: #SCE_MARKDOWN_ULIST_ITEM;
					description: 'list';
					forecolor: (Color fromHTMLSpec: '#33CC00');
					yourself);
		add: ((ScintillaTextStyle name: 14)
					name: #SCE_MARKDOWN_OLIST_ITEM;
					description: 'list';
					forecolor: (Color fromHTMLSpec: '#33CC00');
					yourself);
		add: ((ScintillaTextStyle name: 15)
					name: #SCE_MARKDOWN_BLOCKQUOTE;
					description: 'BLOCKQUOTE';
					yourself);
		add: ((ScintillaTextStyle name: 16)
					name: #SCE_MARKDOWN_STRIKEOUT;
					description: 'STRIKEOUT ~~double tilde~~ (apparently not in Markdown)';
forecolor: Color red;
					font: ((Font default)
								isStruckThrough: true;
								yourself);
					yourself);
		add: ((ScintillaTextStyle name: 17)
					name: #SCE_MARKDOWN_HRULE;
					description: 'HRULE --- (three or more hyphens)';
					yourself);
		add: ((ScintillaTextStyle name: 18)
					name: #SCE_MARKDOWN_LINK;
					description: 'LINK';
					forecolor: (Color fromHTMLSpec: '#0000AA');
					yourself);
		add: ((ScintillaTextStyle name: 19)
					name: #SCE_MARKDOWN_CODE;
					description: 'CODE `single backticks`';
					font: (Font name: 'Consolas');
					backcolor: Color gray;
					yourself);
		add: ((ScintillaTextStyle name: 20)
					name: #SCE_MARKDOWN_CODE2;
					description: 'CODE2 ``double backticks`` (quotes code containing a single backtick)';
					font: (Font name: 'Consolas');
					backcolor: Color gray;
					yourself);
		add: ((ScintillaTextStyle name: 21)
					name: #SCE_MARKDOWN_CODEBK;
					description: 'CODEBLOCK';
					font: (Font name: 'Consolas');
					backcolor: Color gray;
					yourself);
		add: ((ScintillaTextStyle name: 22)
					name: #SCE_MARKDOWN_COMMENT;
					description: 'COMMENT';
					yourself);
		add: ((ScintillaTextStyle name: 23)
					name: #SCE_MARKDOWN_OPTION;
					description: 'OPTION';
					yourself);
		add: ((ScintillaTextStyle name: 24)
					name: #SCE_MARKDOWN_PREPROC;
					description: 'PREPROC';
					yourself);
		add: ((ScintillaTextStyle name: 25)
					name: #SCE_MARKDOWN_POSTPROC;
					description: 'POSTPROC';
					yourself);
		yourself.
	self assert: [answer size < 32].
	"Scintilla pre-defined styles - note how #normal style inherits font of the view (which in turn should be the default system font)"
	answer
		add: ((ScintillaTextStyle name: #indentGuide)
					description: 'Indentation guides, when visible';
					forecolor: Color gray;
					yourself);
		add: ((ScintillaTextStyle name: #braceHighlight)
					description: 'Matching brace, when brace highlighting enabled';
					forecolor: Color blue;
					isBold: true;
					yourself);
		add: ((ScintillaTextStyle name: #braceMismatch)
					description: 'Mismatched brace, when brace highlighting enabled';
					forecolor: Color red;
					isBold: true;
					yourself);
		yourself.
	^answer!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.ScintillaView)  98 50 0 416 98 2 8 1445007428 1025 592 721990 2 ##(Smalltalk.ValueHolder)  0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 5 0 0 0 592 0 8 4294901919 852486 ##(Smalltalk.NullConverter)  0 0 13 0 234 256 98 4 8 #normal 1182726 ##(Smalltalk.ScintillaTextStyle)  1 0 0 1 0 0 0 0 864 0 0 0 8 #callTip 882 77 786694 ##(Smalltalk.IndexedColor)  33554457 946 33554471 1 0 0 0 0 912 0 0 0 98 40 896 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1245510 1 ##(Smalltalk.NullScintillaStyler)  864 234 256 98 2 8 #default 1639942 ##(Smalltalk.ScintillaMarkerDefinition)  1 1 946 33554433 976 592 8 #circle 202 208 544 0 63 9215 0 0 0 0 946 33554447 0 0 65 0 0 0 8 '' 3 234 256 98 2 8 #container 832 0 0 0 0 1 0 234 256 98 6 1 1509190 1 ##(Smalltalk.ScintillaIndicatorStyle)  1 592 65025 3 32 1 0 3 1282 3 592 33423361 5 32 3 0 5 1282 5 592 511 1 32 5 0 0 0 234 256 98 6 8 #Notification 882 1029 0 196934 1 ##(Smalltalk.RGB)  31185883 1 0 0 0 0 1376 0 0 0 8 #Error 882 1031 1410 335 1410 31317503 1 0 0 0 0 1440 0 0 0 8 #Warning 882 1027 1410 60239 1410 31326207 1 0 0 0 0 1504 0 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 12 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 1698 345 501 592 1634 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 592 1634 8 #isTextModified: 98 1 32 592 1634 8 #modificationEventMask: 98 1 9215 592 1634 8 #secondaryCaretForecolor: 98 1 1410 16711423 592 1634 8 #caretPeriod: 98 1 1061 592 1634 8 #wordWrap: 98 1 16 592 1634 8 #margins: 98 1 98 3 984902 1 ##(Smalltalk.ScintillaMargin)  1 592 1 3 32 1 5 2146 3 592 33 1 16 67108863 5 2146 5 592 1 1 16 -67108863 5 592 1634 8 #indentationGuides: 98 1 0 592 1634 8 #tabIndents: 98 1 16 592 1634 8 #tabWidth: 98 1 9 592 1634 8 #areAdditionalCaretsVisible: 98 1 16 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 172 0 0 0 250 0 0 0] 98 0 1698 193 193 0 29 8 'source' 0 1570 202 208 98 1 1634 1664 98 2 1698 7199 21 1698 701 501 416 2402 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 14 0 0 10 0 0 0 109 15 0 0 4 1 0 0] 98 3 592 410 8 ##(Smalltalk.Splitter)  98 12 0 416 98 2 8 1140850688 1 2656 0 738 8 4278190080 0 517 0 0 0 2656 1570 202 208 98 1 1634 1664 98 2 1698 345 1 1698 11 501 2656 2402 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 172 0 0 0 0 0 0 0 177 0 0 0 250 0 0 0] 98 0 2464 0 27 410 8 ##(Smalltalk.SlideyInneyOuteyThing)  98 23 0 416 98 2 8 1409286144 131073 2928 0 738 8 4278190080 0 517 0 0 0 2928 655878 ##(Smalltalk.CardLayout)  202 208 98 2 721414 ##(Smalltalk.Association)  8 'Result' 410 608 98 50 0 410 8 ##(Smalltalk.SlidingCardTray)  98 22 0 2928 98 2 8 1140850688 131073 3184 0 738 3024 0 5 0 0 0 3184 3056 234 256 98 4 3152 8 'markdown' 410 8 ##(Smalltalk.AXControlSite)  98 25 0 3184 98 2 8 1140916224 1 3328 0 738 8 4278190080 0 5 0 0 0 3328 0 537183001 8 'Shell.Explorer' 787462 ##(Smalltalk.IWebBrowser2)  0 0 0 0 918022 ##(Smalltalk.IDolphinAxHost)  0 0 722438 ##(Smalltalk.AXEventSink)  234 240 98 76 213 8 #DownloadBegin 529 8 #WindowSetLeft: 217 8 #ProgressChange:ProgressMax: 531 8 #WindowSetTop: 533 8 #WindowSetWidth: 535 8 #WindowSetHeight: 225 8 #PropertyChange: 227 8 #TitleChange: 527 8 #WindowClosing:Cancel: 537 8 #ClientToHostWindow:CY: 539 8 #SetSecureLockIcon: 541 8 #FileDownload:Cancel: 543 8 #NavigateError:URL:Frame:StatusCode:Cancel: 451 8 #PrintTemplateInstantiation: 453 8 #PrintTemplateTeardown: 455 8 #UpdatePageStatus:nPage:fDone: 545 8 #PrivacyImpactedStateChange: 547 8 #NewWindow3:Cancel:dwFlags:bstrUrlContext:bstrUrl: 565 8 #SetPhishingFilterStatus: 567 8 #WindowStateChanged:dwValidFlagsMask: 569 8 #NewProcess:pWB2:Cancel: 571 8 #ThirdPartyUrlBlocked:dwCount: 573 8 #RedirectXDomainBlocked:StartURL:RedirectURL:Frame:StatusCode: 501 8 #BeforeNavigate2:URL:Flags:TargetFrameName:PostData:Headers:Cancel: 503 8 #NewWindow2:Cancel: 505 8 #NavigateComplete2:URL: 507 8 #OnQuit 509 8 #OnVisible: 511 8 #OnToolBar: 513 8 #OnMenuBar: 515 8 #OnStatusBar: 517 8 #OnFullScreen: 519 8 #DocumentComplete:URL: 521 8 #OnTheaterMode: 205 8 #StatusTextChange: 525 8 #WindowSetResizable: 209 8 #DownloadComplete 211 8 #CommandStateChange:Enable: 3328 1049094 ##(Smalltalk.IConnectionPoint)  0 0 3 1378630 1 ##(Smalltalk.TKindDispatchAnalyzer)  590598 ##(Smalltalk.ITypeInfo)  0 0 525062 ##(Smalltalk.TYPEATTR)  0 4272 4240 0 1378630 2 ##(Smalltalk.AXTypeLibraryAnalyzer)  590342 ##(Smalltalk.ITypeLib2)  0 0 257 524550 ##(Smalltalk.TLIBATTR)  8 #[192 42 178 234 193 48 207 17 167 235 0 0 192 91 174 11 0 0 0 0 1 0 0 0 1 0 1 0 8 0 0 0] 8 '' 8 'Internet Explorer' 8 #SHDocVwLib 8 #SHDocVwConstants 234 240 98 2 8 #GUID 4528 0 0 262198 ##(Smalltalk.GUID)  16 160 21 167 52 135 101 208 17 146 74 0 32 175 199 172 77 25 0 0 0 1 2763 234 240 544 1508358 ##(Smalltalk.IAxWinAmbientDispatchEx)  0 0 0 0 524806 ##(Smalltalk.IUnknown)  0 0 0 0 0 1570 202 208 98 4 1634 1664 98 2 1698 1 37 1698 287 449 3328 1634 8 #restoreAmbientProperties 544 3328 1634 8 #docHostFlags: 98 1 9 3328 1634 8 #controlBinaryStoreBytes: 98 1 8 #[97 249 86 136 10 52 208 17 169 107 0 192 79 215 5 162 76 0 0 0 200 14 0 0 39 23 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 76 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 224 208 87 0 115 53 207 17 174 105 8 0 43 46 18 98 8 0 0 0 0 0 0 0 76 0 0 0 1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70 128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 3328 2402 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 143 0 0 0 242 0 0 0] 98 0 2464 0 27 8 'preview' 0 410 8 ##(Smalltalk.TabViewXP)  98 28 0 2928 98 2 8 1140916866 1 4976 590662 2 ##(Smalltalk.ListModel)  202 208 98 2 8 'Preview' 3136 0 1310726 ##(Smalltalk.IdentitySearchPolicy)  738 3024 0 1 0 0 0 4976 0 8 4294902455 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  1049926 1 ##(Smalltalk.IconImageManager)  0 0 0 0 0 8 #noIcons 0 0 0 0 0 1570 202 208 98 3 1634 1664 98 2 1698 1 1 1698 347 501 4976 1634 8 #basicSelectionsByIndex: 98 1 98 1 3 4976 1634 8 #tcmSetExtendedStyle:dwExStyle: 98 2 -1 1 4976 2402 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 173 0 0 0 250 0 0 0] 98 0 2464 0 27 0 2928 1698 33 33 1049862 ##(Smalltalk.ButtonInteractor)  3184 0 590342 ##(Smalltalk.Rectangle)  1698 5 3 1698 37 35 1 1180998 4 ##(Smalltalk.CommandDescription)  8 #togglePin 8 'Pin or Unpin the tray' 1 1 0 0 0 1570 202 208 98 1 1634 1664 98 2 1698 9 9 1698 287 485 3184 2402 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 4 0 0 0 147 0 0 0 246 0 0 0] 98 2 3328 3152 2464 0 27 98 2 8 1445007428 1025 3152 674 0 32 720 0 738 8 4278190080 0 5 0 0 0 3152 0 8 4294901919 1508358 ##(Smalltalk.MarkdownToHtmlConverter)  0 0 0 0 11 0 234 256 98 8 912 882 77 960 976 1 0 0 0 0 912 0 0 0 8 #indentGuide 882 75 0 0 1 0 0 0 0 6064 0 0 0 8 #lineNumber 882 67 0 0 1 0 0 0 0 6096 0 0 0 864 882 1 0 0 1 0 0 0 0 864 0 0 0 98 40 6128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6112 0 0 0 6080 6048 0 1010 864 234 256 98 2 1072 1090 1 1 1120 976 3152 1136 202 208 544 0 63 9215 0 0 0 0 1168 0 0 65 0 0 0 8 '' 3 234 256 98 2 1232 234 256 98 4 864 882 1 0 0 1 0 0 0 0 864 0 0 0 912 882 77 960 976 1 0 0 0 0 912 0 0 0 0 0 0 0 1 0 234 256 98 6 1 1282 1 3152 65025 3 32 1 0 3 1282 3 3152 33423361 5 32 3 0 5 1282 5 3152 511 1 32 5 0 0 0 234 256 98 6 1376 882 1029 0 1410 31185883 1 0 0 0 0 1376 0 0 0 1440 882 1031 1410 335 1410 31317503 1 0 0 0 0 1440 0 0 0 1504 882 1027 1410 60239 1410 31326207 1 0 0 0 0 1504 0 0 0 0 1570 202 208 98 12 1634 1664 98 2 1698 1 37 1698 287 449 3152 1634 1760 98 1 1794 3 1 3 3152 1634 1840 98 1 32 3152 1634 1888 98 1 9215 3152 1634 1936 98 1 1410 16711423 3152 1634 2000 98 1 1061 3152 1634 2096 98 1 98 3 2146 1 3152 1 3 32 1 5 2146 3 3152 33 1 16 67108863 5 2146 5 3152 1 1 16 -67108863 5 3152 1634 2224 98 1 0 3152 1634 2272 98 1 16 3152 1634 2320 98 1 9 3152 1634 8 #setLexerLanguage: 98 1 8 #hypertext 3152 1634 2368 98 1 16 3152 2402 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 18 0 0 0 143 0 0 0 242 0 0 0] 98 0 2464 0 29 3106 5120 3328 3328 234 256 544 0 4976 3184 1698 201 201 401 1 31 0 0 1570 202 208 98 1 1634 1664 98 2 1698 355 1 1698 347 501 2928 2402 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 177 0 0 0 0 0 0 0 94 1 0 0 250 0 0 0] 98 2 3184 4976 2464 0 27 2464 0 27 )! !
!MarkdownPreviewPresenter class categoriesFor: #defaultModel!models!public! !
!MarkdownPreviewPresenter class categoriesFor: #markdownTextStyles!public! !
!MarkdownPreviewPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

