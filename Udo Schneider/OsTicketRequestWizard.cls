"Filed out from Dolphin Smalltalk 7"!

ValueDialog subclass: #OsTicketRequestWizard
	instanceVariableNames: 'stepsView supportTopicPresenter secondPageTextPresenter subjectPresenter messagePresenter attachSystemInformationPreseneter attachmentPathPresenter namePresenter emailPresenter telephonePresenter summaryPresenter wizardImagePresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OsTicketRequestWizard guid: (GUID fromString: '{e156ef57-5601-44af-8efd-ac8d0b6fdc46}')!
OsTicketRequestWizard comment: ''!
!OsTicketRequestWizard categoriesForClass!MVP-Presenters! !
!OsTicketRequestWizard methodsFor!

addToCommandRoute: route 
	"Update the <OrderedCollection>, path, with the receiver's contribution to the command path
	held by the <CommandPolicy>, route. Answer self to have the command policy decide where
	to go next."

	"Implementation Note: We want to make sure that the Steps card view and the image stripper
	are always in the route"

	route
		appendTarget: stepsView;
		"appendTarget: self model value;"
		appendPresenter: self!

basicCaption
^'Support Wizard'!

browseAttachment
	| filename |
	filename := (FileOpenDialog new)
				caption: 'ï¿½ffne Datei...';
				fileTypes: self model value allowedFileTypes;
				initialDirectory: self model value class defaultErrorsFolder;
				value: attachmentPathPresenter value;
				showModal.
	(filename notNil and: [File exists: filename]) ifTrue: [attachmentPathPresenter value: filename]!

clearAttachment
attachmentPathPresenter value: nil!

createComponents
	super createComponents.
	
	wizardImagePresenter := self add: ImagePresenter new name: 'wizardImage'.
	supportTopicPresenter := self add: RadioButtonSetPresenter new name: 'stepSupportTopic'.
	secondPageTextPresenter := self add: TextPresenter new name: 'secondPageText'.
	subjectPresenter := self add: TextPresenter new name: 'subject'.
	messagePresenter := self add: TextPresenter new name: 'message'.
	attachSystemInformationPreseneter := self add: BooleanPresenter new name: 'attachSystemInformation'.
	attachmentPathPresenter := self add: TextPresenter new name: 'attachmentPath'.
	namePresenter := self add: TextPresenter new name: 'name'.
	emailPresenter := self add: TextPresenter new name: 'email'.
	telephonePresenter := self add: TextPresenter new name: 'telephone'.
	summaryPresenter := self add: TextPresenter new name: 'summary'!

model: aModel 
	| aspectBuffer |
	super model: aModel.
	aspectBuffer := self model.
	supportTopicPresenter model: (aspectBuffer aspectValue: #topic).
	secondPageTextPresenter model: (aspectBuffer aspectValue: #topic).
	subjectPresenter model: (aspectBuffer aspectValue: #subject).
	messagePresenter model: (aspectBuffer aspectValue: #message).
	attachSystemInformationPreseneter model: (aspectBuffer aspectValue: #attachSystemInformation).
	attachmentPathPresenter model: (aspectBuffer aspectValue: #attachmentPath).
	namePresenter model: (aspectBuffer aspectValue: #name).
	emailPresenter model: (aspectBuffer aspectValue: #email).
	telephonePresenter model: (aspectBuffer aspectValue: #phone).
	summaryPresenter model: (aspectBuffer aspectValue: #summary)!

onViewOpened
	"Received when the receiver's view has been connected."

	super onViewOpened.

	"Locate the card view containing the steps. We need to cache this to add it
	to the command routing in #addToCommandRoute:."
	stepsView := self view viewNamed: 'Steps'.
	self caption: self model value productName , ' ' , self basicCaption.
	self view largeIcon: self model value productIcon.
	wizardImagePresenter value: self model value wizardImage.
	(self view allSubViews select: [:each | each name = 'caption']) do: [:each | each text: self model value productName , ' ' , self basicCaption]!

queryCommand: query 
	"Private - Enters details about a potential command for the receiver into the 
	<CommandQuery>,  query."

	| cmd |
	cmd := query commandSymbol.
	cmd == #sendTicket 
		ifTrue: 
			[query isEnabled: self model value isReadyToSent.
			^true].
	^super queryCommand: query!

sendTicket
	| sentRequest |
	sentRequest := self model value submit.
	sentRequest status = 200 
		ifFalse: 
			[
			^(MessageBubble new)
				caption: 'Fehler beim versenden der Daten!!';
				willFade: true;
				timeout: 10000;
				errorMsg: 'Bitte überprüfen Sie Ihre Internetverbindung.'].
	self cancel! !
!OsTicketRequestWizard categoriesFor: #addToCommandRoute:!commands!public! !
!OsTicketRequestWizard categoriesFor: #basicCaption!event handling!public! !
!OsTicketRequestWizard categoriesFor: #browseAttachment!public! !
!OsTicketRequestWizard categoriesFor: #clearAttachment!public! !
!OsTicketRequestWizard categoriesFor: #createComponents!public! !
!OsTicketRequestWizard categoriesFor: #model:!public! !
!OsTicketRequestWizard categoriesFor: #onViewOpened!event handling!public! !
!OsTicketRequestWizard categoriesFor: #queryCommand:!private! !
!OsTicketRequestWizard categoriesFor: #sendTicket!public! !

!OsTicketRequestWizard class methodsFor!

defaultModel
	^OsTicketRequest new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 721158 ##(Smalltalk.SystemColor)  31 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 0 0 0 0 234 256 98 4 410 8 ##(Smalltalk.ImageView)  98 21 0 416 98 2 8 1140850944 1 576 721990 2 ##(Smalltalk.ValueHolder)  0 32 1376774 ##(Smalltalk.PluggableSearchPolicy)  459270 ##(Smalltalk.Message)  8 #= 98 0 722 8 #hash 98 0 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 0 0 0 576 0 0 852486 ##(Smalltalk.NullConverter)  0 0 0 0 8 #centered 1 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 1058 361 741 576 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 180 0 0 0 114 1 0 0] 98 0 1058 193 193 0 27 8 'wizardImage' 410 8 ##(Smalltalk.WizardCardContainer)  98 15 0 416 98 2 8 1140850688 131073 1200 0 786694 ##(Smalltalk.IndexedColor)  33554471 0 1031 0 0 0 1200 655878 ##(Smalltalk.CardLayout)  202 208 98 4 721414 ##(Smalltalk.Association)  9 410 8 ##(Smalltalk.ReferenceView)  98 14 0 1200 98 2 8 1140850688 131073 1408 0 834 8 4278190080 0 7 0 0 0 1408 1180166 ##(Smalltalk.ResourceIdentifier)  8 ##(Smalltalk.OsTicketRequestWizard)  8 #resource_stepSupportTopic 0 930 202 208 98 1 994 1024 98 2 1058 1 1 1058 795 741 1408 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 141 1 0 0 114 1 0 0] 98 0 1168 0 27 1378 9 410 1424 98 14 0 1200 98 2 8 1140850688 131073 1760 0 834 1504 0 5 0 0 0 1760 1522 1552 8 #resource_stepAbout 0 930 202 208 98 1 994 1024 98 2 1058 1 1 1058 795 741 1760 1106 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 141 1 0 0 114 1 0 0] 1728 1168 0 27 1378 9 410 1424 98 14 0 1200 98 2 8 1140850688 131073 2032 0 834 1504 0 5 0 0 0 2032 1522 1552 8 #resource_stepUserInformation 0 930 202 208 98 1 994 1024 98 2 1058 1 1 1058 795 741 2032 1106 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 141 1 0 0 114 1 0 0] 1728 1168 0 27 1378 9 410 1424 98 14 0 1200 98 2 8 1140850688 131073 2304 0 834 1504 0 5 0 0 0 2304 1522 1552 8 #resource_stepSummary 0 930 202 208 98 1 994 1024 98 2 1058 1 1 1058 795 741 2304 1106 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 141 1 0 0 114 1 0 0] 1728 1168 0 27 1408 234 256 98 8 1408 8 'stepSupportTopic' 2032 8 'userInformation' 1760 8 'stepAbout' 2304 8 'stepSummary' 0 930 202 208 98 1 994 1024 98 2 1058 361 1 1058 795 741 1200 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 180 0 0 0 0 0 0 0 65 2 0 0 114 1 0 0] 98 4 1408 1760 2032 2304 1168 0 27 8 'Steps' 590342 ##(Smalltalk.Rectangle)  1058 21 21 1058 21 21 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 32 8 2000955189 930 202 208 98 2 994 1024 98 2 1058 3839 21 1058 1161 881 416 994 8 #updateMenuBar 1728 416 1106 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 195 9 0 0 194 1 0 0] 98 5 1200 576 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 3136 0 0 0 7 0 0 0 3136 0 8 4294902951 1180998 4 ##(Smalltalk.CommandDescription)  8 #previousCard 8 '<< Zurück' 1 1 0 0 32 0 0 0 930 202 208 98 3 994 1024 98 2 1058 551 761 1058 181 51 3136 994 8 #isEnabled: 98 1 32 3136 994 8 #text: 98 1 8 '<< Zurück' 3136 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 19 1 0 0 124 1 0 0 109 1 0 0 149 1 0 0] 98 0 1168 0 29 410 3152 98 20 0 416 98 2 8 1140924416 1 3568 0 0 0 7 0 0 0 3568 0 8 4294902951 3234 8 #nextCard 8 'Weiter >>' 1 1 0 0 32 0 0 0 930 202 208 98 3 994 1024 98 2 1058 741 761 1058 181 51 3568 994 3424 98 1 32 3568 994 3472 98 1 8 'Weiter >>' 3568 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 114 1 0 0 124 1 0 0 204 1 0 0 149 1 0 0] 98 0 1168 0 29 410 3152 98 20 0 416 98 2 8 1140924416 1 3936 0 0 0 7 0 0 0 3936 0 8 4294902951 3234 8 #cancel 8 'Abbruch' 1 1 0 0 32 0 0 0 930 202 208 98 2 994 1024 98 2 1058 951 761 1058 181 51 3936 994 3472 98 1 8 'Abbruch' 3936 1106 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 219 1 0 0 124 1 0 0 53 2 0 0 149 1 0 0] 98 0 1168 0 29 1168 0 27 )!

resource_stepAbout
	"Answer the literal data from which the 'stepAbout' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_stepAbout)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1140850688 131073 416 0 0 0 7 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 24 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 560 0 0 0 7 0 0 0 560 0 8 4294903927 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  17 567 818 759 41 560 754 8 #text: 98 1 8 'Datei:' 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 27 1 0 0 131 1 0 0 47 1 0 0] 98 0 818 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 17 1042 8 #fixedParentRight -19 1042 8 #fixedPreviousBottom 21 1042 8 #fixedViewTop 41 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 1184 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 0 0 0 1184 0 8 4294903385 658 0 8 'Bitte geben Sie hier einen Betreff ein' 1 690 202 208 98 5 754 784 98 2 818 17 257 818 763 41 1184 754 880 98 1 8 'Bitte geben Sie hier einen Betreff ein' 1184 754 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1184 754 8 #isTextModified: 98 1 32 1184 754 8 #setMarginWidths: 98 1 98 2 7 7 1184 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 128 0 0 0 133 1 0 0 148 0 0 0] 98 0 992 0 27 1010 1056 17 1088 -15 1120 1 1152 41 410 8 ##(Smalltalk.CheckBox)  98 16 0 416 98 2 8 1409363203 1 1776 721990 2 ##(Smalltalk.ValueHolder)  0 0 1114118 ##(Smalltalk.NeverSearchPolicy)  32 0 0 7 0 0 0 1776 0 8 4294903371 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 507 818 763 41 1776 754 880 98 1 8 'Systeminformationen anhängen' 1776 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 253 0 0 0 133 1 0 0 17 1 0 0] 98 0 992 0 27 1010 1056 17 1088 -15 1120 1 1152 41 410 1200 98 16 0 416 98 2 8 1140916352 1025 2176 0 1266 1296 0 7 0 0 0 2176 0 8 4294903385 658 0 8 'Keine Datei ausgewählt' 3 690 202 208 98 5 754 784 98 2 818 17 607 818 539 41 2176 754 880 98 1 8 'Keine Datei ausgewählt' 2176 754 1536 98 1 1570 3 1 3 2176 754 1616 98 1 32 2176 754 1664 98 1 98 2 7 7 2176 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 47 1 0 0 21 1 0 0 67 1 0 0] 98 0 992 0 27 1010 1056 17 1088 -239 1120 1 1152 41 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 2656 0 0 0 7 0 0 0 2656 0 8 4294903371 1180998 4 ##(Smalltalk.CommandDescription)  8 #browseAttachment 8 'Suchen' 1 1 0 0 32 0 0 0 690 202 208 98 3 754 784 98 2 818 565 607 818 101 41 2656 754 8 #isEnabled: 98 1 32 2656 754 880 98 1 8 'Suchen' 2656 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 26 1 0 0 47 1 0 0 76 1 0 0 67 1 0 0] 98 0 992 0 29 1010 1042 8 #fixedPreviousRight 11 1042 8 #fixedViewLeft 101 1042 8 #fixedPreviousTop 1 1120 1 410 2672 98 20 0 416 98 2 8 1140924416 1 3184 0 0 0 7 0 0 0 3184 0 8 4294903371 2754 8 #clearAttachment 8 'Löschen' 1 1 0 0 32 0 0 0 690 202 208 98 3 754 784 98 2 818 675 607 818 111 41 3184 754 2944 98 1 32 3184 754 880 98 1 8 'Löschen' 3184 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 81 1 0 0 47 1 0 0 136 1 0 0 67 1 0 0] 98 0 992 0 29 1010 3088 11 1088 -9 3152 1 1120 1 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 416 98 2 8 1143017796 1025 3568 0 1266 8 4278190080 0 7 0 0 0 3568 0 8 4294903385 658 0 8 'Bitte geben Sie hier weitere Informationen zu Ihrer Anfrage ein' 9 690 202 208 98 5 754 784 98 2 818 17 357 818 763 151 3568 754 880 98 1 8 'Bitte geben Sie hier weitere Informationen zu Ihrer Anfrage ein' 3568 754 1536 98 1 1570 3 1 3 3568 754 1616 98 1 32 3568 754 1664 98 1 98 2 7 7 3568 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 178 0 0 0 133 1 0 0 253 0 0 0] 98 0 992 0 27 1010 1056 17 1088 -15 1120 1 1152 151 410 576 98 16 0 416 98 2 8 1140850944 1 4080 0 0 0 7 0 0 0 4080 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 217 818 759 41 4080 754 880 98 1 8 'Betreff:' 4080 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 108 0 0 0 131 1 0 0 128 0 0 0] 98 0 992 0 27 1010 1056 17 1088 -19 1120 21 1152 41 410 576 98 16 0 416 98 2 8 1140850946 1 4400 0 0 0 7 0 0 0 4400 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 297 697 818 481 37 4400 754 880 98 1 8 'Bitte klicken Sie "Weiter" um fortzufahren' 4400 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 148 0 0 0 92 1 0 0 132 1 0 0 110 1 0 0] 98 0 992 0 27 1010 1056 297 3120 481 1042 8 #fixedParentTop 697 1152 37 410 576 98 16 0 416 98 2 8 1140850945 1 4752 0 0 0 7 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[235 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 818 193 193 0 4752 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 21 818 763 61 4752 754 880 98 1 8 '
' 4752 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 10 0 0 0 133 1 0 0 40 0 0 0] 98 0 992 0 27 1010 1056 17 1088 -15 4720 21 1152 61 410 576 98 16 0 416 98 2 8 1140850944 1 5168 0 0 0 7 0 0 0 5168 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 317 818 759 41 5168 754 880 98 1 8 'Weitere Informationen:' 5168 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 158 0 0 0 131 1 0 0 178 0 0 0] 98 0 992 0 27 1010 1056 17 1088 -19 1120 21 1152 41 410 576 98 16 0 416 98 2 8 1140850944 1 5488 0 0 0 7 0 0 0 5488 0 8 4294903927 1049606 ##(Smalltalk.MappingConverter)  0 0 202 8 ##(Smalltalk.Dictionary)  98 3 721414 ##(Smalltalk.Association)  8 #general 8 'Geben Sie uns bitte ein paar Hintergrund-Informationen, damit wir gezielt auf Ihre Fragen bzw. Anregungen eingehen können.' 5650 8 #billing 8 'Geben Sie uns bitte ein paar Hintergrund-Informationen zur Lizensierung/Vertrieb. Habe Sie die Kauf-Bestätigungmail bekommen? Wo haben Sie die Lizenz gekauft? Wie haben Sie die Lizenz eingespielt?' 5650 8 #support 8 'Geben Sie uns bitte ein paar Hintergrund-Informationen zum Fehlverhalten. Was haben Sie getan um den Fehler hervorzurufen? Lässt sich dieses Verhalten reproduzieren? Mit welchen Eingaben/Dateien?' 202 5616 98 3 5650 5696 5680 5650 5744 5728 5650 5792 5776 0 690 202 208 98 1 754 784 98 2 818 17 97 818 759 101 5488 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 48 0 0 0 131 1 0 0 98 0 0 0] 98 0 992 0 27 1010 1056 17 1088 -19 1120 17 1152 101 234 256 98 12 1184 8 'subject' 1776 8 'attachSystemInformation' 2176 8 'attachmentPath' 3568 8 'message' 5488 8 'secondPageText' 4752 8 'caption' 0 690 202 208 98 1 754 784 98 2 818 3839 21 818 795 741 416 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 12 9 0 0 124 1 0 0] 98 12 4752 5488 4080 1184 5168 3568 1776 560 2176 2656 3184 4400 992 0 27 )!

resource_stepSummary
	"Answer the literal data from which the 'stepSummary' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_stepSummary)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1140850688 131073 416 0 0 0 7 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 10 410 8 ##(Smalltalk.PushButton)  98 20 0 416 98 2 8 1140924416 1 560 0 0 0 7 0 0 0 560 0 8 4294903371 1180998 4 ##(Smalltalk.CommandDescription)  8 #sendTicket 8 'Senden...' 1 1 0 0 32 0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  635 657 850 145 51 560 786 8 #isEnabled: 98 1 32 560 786 8 #text: 98 1 8 'Senden...' 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 61 1 0 0 72 1 0 0 133 1 0 0 97 1 0 0] 98 0 850 193 193 0 29 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentRight -159 1136 -15 1122 8 #fixedPreviousBottom 11 1122 8 #fixedViewTop 51 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850944 1 1232 0 0 0 7 0 0 0 1232 0 8 4294903927 852486 ##(Smalltalk.NullConverter)  0 0 0 722 202 208 98 2 786 816 98 2 850 17 267 850 759 41 1232 786 960 98 1 8 'Zusammenfassung:
' 1232 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 133 0 0 0 131 1 0 0 153 0 0 0] 98 0 1072 0 27 1090 1122 8 #fixedParentLeft 17 1136 -19 1168 21 1200 41 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 416 98 2 8 1143017796 1025 1616 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 0 0 0 1616 0 8 4294903385 1330 0 0 11 722 202 208 98 4 786 816 98 2 850 17 307 850 763 341 1616 786 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1616 786 8 #isTextModified: 98 1 32 1616 786 8 #setMarginWidths: 98 1 98 2 7 7 1616 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 153 0 0 0 133 1 0 0 67 1 0 0] 98 0 1072 0 27 1090 1584 17 1136 -15 1168 1 1200 341 410 1248 98 16 0 416 98 2 8 1140850945 1 2144 0 0 0 7 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[235 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 850 193 193 0 2144 0 8 4294903927 1330 0 0 0 722 202 208 98 2 786 816 98 2 850 17 21 850 763 61 2144 786 960 98 1 8 '
' 2144 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 10 0 0 0 133 1 0 0 40 0 0 0] 98 0 1072 0 27 1090 1584 17 1136 -15 1122 8 #fixedParentTop 21 1200 61 410 1248 98 16 0 416 98 2 8 1140850944 1 2592 0 0 0 7 0 0 0 2592 0 8 4294903927 1330 0 0 0 722 202 208 98 2 786 816 98 2 850 17 97 850 759 151 2592 786 960 98 1 8 'Die Zusammenfassung zeigt, welche Daten an unser Support-Team gesendet werden. Möchten Sie diese noch einmal überarbeiten, klicken Sie "Zurück". Sind die Angaben jedoch bereits vollständig und korrekt, gehen Sie bitte auf "Senden"
' 2592 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 48 0 0 0 131 1 0 0 123 0 0 0] 98 0 1072 0 27 1090 1584 17 1136 -19 1168 17 1200 151 234 256 98 6 2144 8 'caption' 1616 8 'summary' 560 8 'sendTicket' 0 722 202 208 98 1 786 816 98 2 850 3839 21 850 795 741 416 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 12 9 0 0 124 1 0 0] 98 5 2144 2592 1232 1616 560 1072 0 27 )!

resource_stepSupportTopic
	"Answer the literal data from which the 'stepSupportTopic' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_stepSupportTopic)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1140981760 131073 416 0 0 0 7 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 14 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850946 1 560 0 0 0 7 0 0 0 560 0 8 4294903927 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  297 697 818 481 37 560 754 8 #text: 98 1 8 'Bitte klicken Sie "Weiter" um fortzufahren' 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 148 0 0 0 92 1 0 0 132 1 0 0 110 1 0 0] 98 0 818 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 297 1042 8 #fixedViewLeft 481 1042 8 #fixedParentTop 697 1042 8 #fixedViewTop 37 410 576 98 16 0 416 98 2 8 1140850944 1 1184 0 0 0 7 0 0 0 1184 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 67 427 818 709 91 1184 754 880 98 1 8 'Sie haben technische Fragen zum Produkt? Während der Programmausführung treten Fehler auf oder das Programm verhält sich nicht wie gewünscht/erwartet?' 1184 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 33 0 0 0 213 0 0 0 131 1 0 0 2 1 0 0] 98 0 992 0 27 1010 1056 67 1042 8 #fixedParentRight -19 1042 8 #fixedPreviousBottom -3 1152 91 410 576 98 16 0 416 98 2 8 1140850944 1 1568 0 0 0 7 0 0 0 1568 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 67 331 818 709 31 1568 754 880 98 1 8 'Sie haben allgemeine Fragen, Anregungen o.ä.?' 1568 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 33 0 0 0 165 0 0 0 131 1 0 0 180 0 0 0] 98 0 992 0 27 1010 1056 67 1504 -19 1536 1 1152 31 410 8 ##(Smalltalk.RadioButton)  98 16 0 416 98 2 8 1140927753 1 1888 721990 2 ##(Smalltalk.ValueHolder)  0 0 1376774 ##(Smalltalk.PluggableSearchPolicy)  459270 ##(Smalltalk.Message)  8 #= 98 0 2034 8 #hash 98 0 32 0 0 7 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 818 193 193 0 1888 0 8 4294903371 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 271 818 763 61 1888 754 880 98 1 8 'Allgemeines' 1888 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 135 0 0 0 133 1 0 0 165 0 0 0] 98 0 992 0 27 1010 1056 17 1504 -15 1536 11 1152 61 410 1904 98 16 0 416 98 2 8 1140927753 1 2496 1970 0 0 2002 2034 2064 98 0 2034 2112 98 0 32 0 0 7 0 2146 0 16 2178 2208 818 193 193 0 2496 0 8 4294903371 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 371 818 763 61 2496 754 880 98 1 8 'Technischer Support' 2496 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 185 0 0 0 133 1 0 0 215 0 0 0] 98 0 992 0 27 1010 1056 17 1504 -15 1536 11 1152 61 410 576 98 16 0 416 98 2 8 1140850945 1 2960 0 0 0 7 0 2146 0 16 2178 8 #[235 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 818 193 193 0 2960 0 8 4294903927 658 0 0 0 690 202 208 98 1 754 784 98 2 818 17 21 818 763 61 2960 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 10 0 0 0 133 1 0 0 40 0 0 0] 98 0 992 0 27 1010 1056 17 1504 -15 1120 21 1152 61 410 576 98 16 0 416 98 2 8 1140850944 1 3296 0 0 0 7 0 0 0 3296 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 101 818 763 161 3296 754 880 98 1 8 'Dieser Assistent hilft Ihnen alle benötigten Daten für einen erfolgreichen Support zu sammeln.

Natürlich haben Sie vor dem abschließenden Versand der Daten noch einmal die Möglichkeit genau zu sehen, was versendet wird.

Welcher Art von Support-Anfarge haben Sie?' 3296 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 50 0 0 0 133 1 0 0 130 0 0 0] 98 0 992 0 27 1010 1056 17 1504 -15 1536 21 1152 161 234 256 98 6 1888 8 'general' 2496 8 'support' 2960 8 'caption' 0 690 202 208 98 1 754 784 98 2 818 3839 21 818 795 741 416 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 12 9 0 0 124 1 0 0] 98 7 2960 3296 1888 1568 2496 1184 560 992 0 27 )!

resource_stepUserInformation
	"Answer the literal data from which the 'stepUserInformation' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_stepUserInformation)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ContainerView)  98 15 0 0 98 2 8 1140850688 131073 416 0 0 0 7 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 18 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850946 1 560 0 0 0 7 0 0 0 560 0 8 4294903927 852486 ##(Smalltalk.NullConverter)  0 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 2 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  297 697 818 481 37 560 754 8 #text: 98 1 8 'Bitte klicken Sie "Weiter" um fortzufahren' 560 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 148 0 0 0 92 1 0 0 132 1 0 0 110 1 0 0] 98 0 818 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 297 1042 8 #fixedViewLeft 481 1042 8 #fixedParentTop 697 1042 8 #fixedViewTop 37 410 8 ##(Smalltalk.TextEdit)  98 16 0 416 98 2 8 1140916352 1025 1184 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 7 0 0 0 1184 0 8 4294903385 658 0 8 'Bitte geben Sie hier Ihren Namen ein' 1 690 202 208 98 5 754 784 98 2 818 17 257 818 763 41 1184 754 880 98 1 8 'Bitte geben Sie hier Ihren Namen ein' 1184 754 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1184 754 8 #isTextModified: 98 1 32 1184 754 8 #setMarginWidths: 98 1 98 2 7 7 1184 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 128 0 0 0 133 1 0 0 148 0 0 0] 98 0 992 0 27 1010 1056 17 1042 8 #fixedParentRight -15 1042 8 #fixedPreviousBottom 1 1152 41 410 576 98 16 0 416 98 2 8 1140850944 1 1840 0 0 0 7 0 0 0 1840 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 317 818 759 41 1840 754 880 98 1 8 'Email:' 1840 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 158 0 0 0 131 1 0 0 178 0 0 0] 98 0 992 0 27 1010 1056 17 1776 -19 1808 21 1152 41 410 576 98 16 0 416 98 2 8 1140850944 1 2160 0 0 0 7 0 0 0 2160 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 97 818 759 101 2160 754 880 98 1 8 'Wie können wir Sie am besten erreichen?
Selbstverständlich behandeln wir Ihre Daten vertraulich und verwenden sie ausschließlich zu internen Zwecken.' 2160 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 48 0 0 0 131 1 0 0 98 0 0 0] 98 0 992 0 27 1010 1056 17 1776 -19 1808 17 1152 101 410 1200 98 16 0 416 98 2 8 1140916352 1025 2480 0 1266 1296 0 7 0 0 0 2480 0 8 4294903385 658 0 8 'Bitte geben Sie hier Ihre Telefonnummer ein' 1 690 202 208 98 5 754 784 98 2 818 17 457 818 763 41 2480 754 880 98 1 8 'Bitte geben Sie hier Ihre Telefonnummer ein' 2480 754 1536 98 1 1570 3 1 3 2480 754 1616 98 1 32 2480 754 1664 98 1 98 2 7 7 2480 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 228 0 0 0 133 1 0 0 248 0 0 0] 98 0 992 0 27 1010 1056 17 1776 -15 1808 1 1152 41 410 576 98 16 0 416 98 2 8 1140850944 1 2960 0 0 0 7 0 0 0 2960 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 417 818 759 41 2960 754 880 98 1 8 'Telefon (Optional):' 2960 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 208 0 0 0 131 1 0 0 228 0 0 0] 98 0 992 0 27 1010 1056 17 1776 -19 1808 21 1152 41 410 576 98 16 0 416 98 2 8 1140850945 1 3280 0 0 0 7 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[235 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 818 193 193 0 3280 0 8 4294903927 658 0 0 0 690 202 208 98 1 754 784 98 2 818 17 21 818 763 61 3280 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 10 0 0 0 133 1 0 0 40 0 0 0] 98 0 992 0 27 1010 1056 17 1776 -15 1120 21 1152 61 410 1200 98 16 0 416 98 2 8 1140916352 1025 3648 0 1266 1296 0 7 0 0 0 3648 0 8 4294903385 658 0 8 'Bitte geben Sie hier Ihre E-Mail Adresse ein' 1 690 202 208 98 5 754 784 98 2 818 17 357 818 763 41 3648 754 880 98 1 8 'Bitte geben Sie hier Ihre E-Mail Adresse ein' 3648 754 1536 98 1 1570 3 1 3 3648 754 1616 98 1 32 3648 754 1664 98 1 98 2 7 7 3648 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 178 0 0 0 133 1 0 0 198 0 0 0] 98 0 992 0 27 1010 1056 17 1776 -15 1808 1 1152 41 410 576 98 16 0 416 98 2 8 1140850944 1 4128 0 0 0 7 0 0 0 4128 0 8 4294903927 658 0 0 0 690 202 208 98 2 754 784 98 2 818 17 217 818 759 41 4128 754 880 98 1 8 'Name:
' 4128 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 108 0 0 0 131 1 0 0 128 0 0 0] 98 0 992 0 27 1010 1056 17 1776 -19 1808 21 1152 41 234 256 98 8 3648 8 'email' 2480 8 'telephone' 3280 8 'caption' 1184 8 'name' 0 690 202 208 98 1 754 784 98 2 818 3839 21 818 795 741 416 930 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 12 9 0 0 124 1 0 0] 98 9 3280 2160 4128 1184 1840 3648 2960 2480 560 992 0 27 )! !
!OsTicketRequestWizard class categoriesFor: #defaultModel!public! !
!OsTicketRequestWizard class categoriesFor: #resource_Default_view!public!resources-views! !
!OsTicketRequestWizard class categoriesFor: #resource_stepAbout!must not strip!public!resources-views! !
!OsTicketRequestWizard class categoriesFor: #resource_stepSummary!must not strip!public!resources-views! !
!OsTicketRequestWizard class categoriesFor: #resource_stepSupportTopic!must not strip!public!resources-views! !
!OsTicketRequestWizard class categoriesFor: #resource_stepUserInformation!must not strip!public!resources-views! !

