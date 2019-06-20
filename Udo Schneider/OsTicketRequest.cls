"Filed out from Dolphin Smalltalk 7"!

Object subclass: #OsTicketRequest
	instanceVariableNames: 'name email phone topicIndex subject message priorityIndex attachmentPath attachSystemInformation'
	classVariableNames: ''
	poolDictionaries: 'SHDocVwConstants'
	classInstanceVariableNames: ''!
OsTicketRequest guid: (GUID fromString: '{1763cd53-86b7-47cb-99cb-5aae068fce0e}')!
OsTicketRequest comment: ''!
!OsTicketRequest categoriesForClass!Kernel-Objects! !
!OsTicketRequest methodsFor!

allowedFileTypes
	^#(#('Crashdumps (*.errors)' '*.errors')  )!

attachmentFileExists
	^File exists: self attachmentPath!

attachmentPath
	^attachmentPath!

attachmentPath: aString 
	attachmentPath := aString!

attachmentTypeIsValid
	| attachmentType |
	attachmentType := '.' , (File splitExtensionFrom: self attachmentPath) asLowercase.
	^self allowedFileTypes anySatisfy: [:each | each second asLowercase endsWith: attachmentType]!

attachSystemInformation
	^attachSystemInformation!

attachSystemInformation: aBoolean 
	attachSystemInformation := aBoolean!

email
	^email!

email: aString 
	email := aString!

fullMessage
	| stream |
	stream := ReadWriteStream on: String new.
	attachSystemInformation 
		ifTrue: 
			[self
				printSystemInformationOn: stream. stream
				cr;
				cr].
	stream nextPutAll: self message.
	^stream contents!

initialize
	super initialize.
	name :=nil.
	email := nil.
	phone := nil.
	topicIndex := self class topicMapping at: #general.
	subject := nil.
	message :=nil.
	priorityIndex := self class priorityMapping at: #normal.
	attachmentPath := nil.
	attachSystemInformation := true!

isAttachmentValid
	self attachmentPath isEmptyOrNil ifTrue: [^true].
	self attachmentFileExists ifFalse: [^false].
	^self attachmentTypeIsValid!

isEmailValid
	| parts |
	email isEmptyOrNil ifTrue: [^false].
	parts := email subStrings: $@.
	parts size = 2 ifFalse: [^false].
	parts := parts second subStrings: $..
	parts size = 1 ifTrue: [^false].
	^true!

isReadyToSent
	name isEmptyOrNil ifTrue: [^false].
	subject isEmptyOrNil ifTrue: [^false].
	message isEmptyOrNil ifTrue: [^false].
	self isAttachmentValid ifFalse: [^false].
	self isEmailValid ifFalse: [^false].
	^true!

isSupportTicket
^self topic = #support!

message
	^message!

message: aString 
	message := aString!

name
	^name!

name: aString 
	name := aString!

phone
	^phone!

phone: aString 
	phone := aString!

postData
	| builder |
	builder := (HttpPostDataBuilder new)
				at: #name putValue: self name;
				at: #email putValue: self email;
				at: #phone putValue: self phone;
				at: #topicId putValue: self topicIndex;
				at: #subject putValue: self subject;
				at: #message putValue: self fullMessage ;
				at: #pri putValue: self priorityIndex;
				yourself.
	(self attachmentPath notNilOrEmpty and: [File exists: self attachmentPath]) 
		ifTrue: [builder at: #attachment putFile: self attachmentPath].
	^builder encodedResult!

printAttachmentErrorOn: aStream 
	aStream
		nextPutAll: 'Anhang nicht gefunden oder ungültiger Anhang (nicht unterstütztes Format)!!';
		cr.
		self attachmentFileExists ifFalse: [
			aStream  nextPutAll: 'Datei existiert nicht: '; nextPutAll: self attachmentPath ;cr.].
	self attachmentTypeIsValid 
		ifFalse: 
			[aStream nextPutAll: 'Erlaubte Formate: '.
			self allowedFileTypes do: [:each | aStream nextPutAll: each first]
				separatedBy: [aStream nextPutAll: ', '].
			aStream cr].
	aStream cr!

printEmailErrorOn: aStream 
	aStream
		nextPutAll: 'EMail Adresse (' , self email displayString , ') ist ungültig!!';
		cr;
		cr!

printErrorOn: aStream
	aStream
		nextPutAll: 'FEHLENDE ANGABEN!!';
		cr;
		nextPutAll: 'Ticket kann nicht erÃ¶ffnet werden.';
		cr;
		cr.
	name isEmptyOrNil ifTrue: [self printNameErrorOn: aStream].
	self isEmailValid ifFalse: [self printEmailErrorOn: aStream].
	subject isEmptyOrNil ifTrue: [self printSubjectErrorOn: aStream].
	message isEmptyOrNil ifTrue: [self printMessageErrorOn: aStream].
	self isAttachmentValid ifFalse: [self printAttachmentErrorOn: aStream]!

printMessageErrorOn: aStream 
	aStream
		nextPutAll: 'Keine Nachricht angegeben!!';
		cr;
		cr!

printNameErrorOn: aStream 
	aStream
		nextPutAll: 'Kein Name angegeben!!';
		cr;
		cr!

printSubjectErrorOn: aStream 
	aStream
		nextPutAll: 'Kein Betreff angegeben!!';
		cr;
		cr!

printSystemInformationOn: aStream 
	| ov |
	ov := OSVERSIONINFO current.
	aStream
		nextPutAll: 'Betriebssystem: ';
		nextPutAll: ov dwMajorVersion displayString;
		nextPut: $.;
		nextPutAll: ov dwMinorVersion displayString;
		nextPut: Character space;
		nextPutAll: ov szCSDVersion displayString;
		nextPutAll: ' (Build ';
		nextPutAll: ov dwBuildNumber displayString;
		nextPut: $);
		cr.
	aStream
		nextPutAll: 'Benutzer: ';
		nextPutAll: SessionManager current userName , '@' , SessionManager current computerName;
		cr.
	aStream nextPutAll: 'Monitore: '.
	DesktopView current allDisplayMonitors 
		do: [:eachRectangle | aStream nextPutAll: eachRectangle extent displayString]
		separatedBy: [aStream nextPutAll: ', '].
	aStream cr!

printValidSummaryOn: aStream 
	aStream
		nextPutAll: 'Support URL:';
		cr;
		nextPutAll: self supportUrl;
		cr;
		cr.
	aStream
		nextPutAll: 'Name: ';
		cr;
		nextPutAll: self name;
		cr;
		cr.
	aStream
		nextPutAll: 'Email: ';
		cr;
		nextPutAll: self email;
		cr;
		cr.
	aStream
		nextPutAll: 'Art: ';
		cr;
		nextPutAll: self topic;
		cr;
		cr.
	aStream
		nextPutAll: 'Proritï¿½t: ';
		cr;
		nextPutAll: self priority;
		cr;
		cr.
	aStream
		nextPutAll: 'Betreff: ';
		cr;
		nextPutAll: self subject;
		cr;
		cr.
	self attachmentPath notNilOrEmpty 
		ifTrue: 
			[aStream
				nextPutAll: 'Datei: ';
				cr;
				nextPutAll: self attachmentPath;
				cr;
				cr].
	aStream
		nextPutAll: 'Nachricht: ';
		cr;
		nextPutAll: self message;
		cr;
		cr.
	attachSystemInformation 
		ifTrue: 
			[aStream
				nextPutAll: 'System Informationen: ';
				cr;
				nextPutAll: self systemInformation;
				cr;
				cr]!

priority
^self class priorityMapping keyAtValue: self priorityIndex!

priority: aSymbol
self priorityIndex: (self class priorityMapping at: aSymbol)!

priorityEmergency
	self priority: #emergency!

priorityHigh
	self priority: #high!

priorityIndex
	^priorityIndex!

priorityIndex: anInteger 
	priorityIndex := anInteger!

priorityLow
self priority: #low.!

priorityNormal
	self priority: #normal!

productIcon
^nil!

productName
^'UNKNOWN PRODUCT'!

subject
	^subject!

subject: aString 
	subject := aString!

submit
	| request |
	Cursor wait showWhile: 
			[| postData |
			postData := self postData.
			request := IXMLHttpRequest newXMLHTTP.
			request open: 'POST' bstrUrl: self supportUrl.
			postData key 
				keysAndValuesDo: [:headerName :headerValue | request SetRequestHeader: headerName bstrValue: headerValue].
			request send: postData value.
			[request readyState == READYSTATE_COMPLETE] whileFalse: [SessionManager inputState pumpMessages]].
	^request !

summary
	| summary |
	summary := ReadWriteStream on: String new.
	self isReadyToSent ifTrue: [self printValidSummaryOn: summary.] ifFalse: [self printErrorOn: summary ].
	
	^summary contents!

supportUrl
	^self class baseUrl , '/open.php'!

systemInformation
	| aStream  |
	aStream := ReadWriteStream on: String new.
	self printSystemInformationOn: aStream.
	^aStream contents!

topic
	^self class topicMapping keyAtValue: self topicIndex!

topic: aSymbol 
	self topicIndex:  (self class topicMapping at: aSymbol)!

topicGeneral
self topic: #general!

topicIndex
	^topicIndex!

topicIndex: anInteger 
	topicIndex := anInteger!

topicSupport
self topic: #support!

wizardImage
^OLEPicture fromFile: 'Resources\support.jpg'! !
!OsTicketRequest categoriesFor: #allowedFileTypes!public! !
!OsTicketRequest categoriesFor: #attachmentFileExists!accessing!private!testing! !
!OsTicketRequest categoriesFor: #attachmentPath!accessing!public! !
!OsTicketRequest categoriesFor: #attachmentPath:!accessing!public! !
!OsTicketRequest categoriesFor: #attachmentTypeIsValid!accessing!private!testing! !
!OsTicketRequest categoriesFor: #attachSystemInformation!accessing!public! !
!OsTicketRequest categoriesFor: #attachSystemInformation:!accessing!public! !
!OsTicketRequest categoriesFor: #email!accessing!public! !
!OsTicketRequest categoriesFor: #email:!accessing!public! !
!OsTicketRequest categoriesFor: #fullMessage!helpers!private! !
!OsTicketRequest categoriesFor: #initialize!initialize/release!private! !
!OsTicketRequest categoriesFor: #isAttachmentValid!accessing!private!testing! !
!OsTicketRequest categoriesFor: #isEmailValid!accessing!private!testing! !
!OsTicketRequest categoriesFor: #isReadyToSent!accessing!public!testing! !
!OsTicketRequest categoriesFor: #isSupportTicket!public!testing! !
!OsTicketRequest categoriesFor: #message!accessing!public! !
!OsTicketRequest categoriesFor: #message:!accessing!public! !
!OsTicketRequest categoriesFor: #name!accessing!public! !
!OsTicketRequest categoriesFor: #name:!accessing!public! !
!OsTicketRequest categoriesFor: #phone!accessing!public! !
!OsTicketRequest categoriesFor: #phone:!accessing!public! !
!OsTicketRequest categoriesFor: #postData!helpers!private! !
!OsTicketRequest categoriesFor: #printAttachmentErrorOn:!accessing!private! !
!OsTicketRequest categoriesFor: #printEmailErrorOn:!accessing!private! !
!OsTicketRequest categoriesFor: #printErrorOn:!accessing!private! !
!OsTicketRequest categoriesFor: #printMessageErrorOn:!accessing!private! !
!OsTicketRequest categoriesFor: #printNameErrorOn:!accessing!private! !
!OsTicketRequest categoriesFor: #printSubjectErrorOn:!accessing!private! !
!OsTicketRequest categoriesFor: #printSystemInformationOn:!helpers!private! !
!OsTicketRequest categoriesFor: #printValidSummaryOn:!accessing!private! !
!OsTicketRequest categoriesFor: #priority!accessing!public! !
!OsTicketRequest categoriesFor: #priority:!accessing!public! !
!OsTicketRequest categoriesFor: #priorityEmergency!accessing!public! !
!OsTicketRequest categoriesFor: #priorityHigh!accessing!public! !
!OsTicketRequest categoriesFor: #priorityIndex!accessing!public! !
!OsTicketRequest categoriesFor: #priorityIndex:!accessing!public! !
!OsTicketRequest categoriesFor: #priorityLow!accessing!public! !
!OsTicketRequest categoriesFor: #priorityNormal!accessing!public! !
!OsTicketRequest categoriesFor: #productIcon!public! !
!OsTicketRequest categoriesFor: #productName!public! !
!OsTicketRequest categoriesFor: #subject!accessing!public! !
!OsTicketRequest categoriesFor: #subject:!accessing!public! !
!OsTicketRequest categoriesFor: #submit!actions!public! !
!OsTicketRequest categoriesFor: #summary!accessing!public! !
!OsTicketRequest categoriesFor: #supportUrl!actions!public! !
!OsTicketRequest categoriesFor: #systemInformation!helpers!private! !
!OsTicketRequest categoriesFor: #topic!accessing!public! !
!OsTicketRequest categoriesFor: #topic:!accessing!public! !
!OsTicketRequest categoriesFor: #topicGeneral!accessing!public! !
!OsTicketRequest categoriesFor: #topicIndex!accessing!public! !
!OsTicketRequest categoriesFor: #topicIndex:!accessing!public! !
!OsTicketRequest categoriesFor: #topicSupport!public! !
!OsTicketRequest categoriesFor: #wizardImage!public! !

!OsTicketRequest class methodsFor!

baseUrl
	self subclassResponsibility!

defaultApplicationName
	^SessionManager current applicationName!

defaultErrorsFilename
	^File splitFilenameFrom:  self defaultErrorsPath!

defaultErrorsFolder
	^File splitPathFrom:  self defaultErrorsPath!

defaultErrorsPath
	^SessionManager current imagePath , '.errors'!

existsErrorFile
^(File exists: self defaultErrorsPath)!

new
^super new initialize!

priorityMapping
	^##((Dictionary new)
		at: #low put: 1;
		at: #normal put: 2;
		at: #high put: 3;
		at: #emergency put: 4;
		yourself)!

topicMapping
	^##((Dictionary new)
		at: #general put: 0;
		at: #support put: 1;
	
		yourself)! !
!OsTicketRequest class categoriesFor: #baseUrl!constants!public! !
!OsTicketRequest class categoriesFor: #defaultApplicationName!public! !
!OsTicketRequest class categoriesFor: #defaultErrorsFilename!public! !
!OsTicketRequest class categoriesFor: #defaultErrorsFolder!public! !
!OsTicketRequest class categoriesFor: #defaultErrorsPath!public! !
!OsTicketRequest class categoriesFor: #existsErrorFile!public! !
!OsTicketRequest class categoriesFor: #new!instance creation!public! !
!OsTicketRequest class categoriesFor: #priorityMapping!public! !
!OsTicketRequest class categoriesFor: #topicMapping!public! !

