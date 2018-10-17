"Filed out from Dolphin Smalltalk 7"!

Win32Structure subclass: #NOTIFYICONDATA2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NOTIFYICONDATA2 guid: (GUID fromString: '{8eaa50af-77a9-4544-b6b8-1a9effb83299}')!
NOTIFYICONDATA2 comment: ''!
!NOTIFYICONDATA2 categoriesForClass!Unclassified! !
!NOTIFYICONDATA2 methodsFor!

balloonFlags: anInteger 
	self dwInfoFlags: anInteger.
	self uFlags: (self uFlags bitOr: NIF_INFO)!

balloonMessage: aString 
	self szInfo: aString.
	self uFlags: (self uFlags bitOr: NIF_INFO)!

balloonTimeout: anInteger 
	self uTimeout: anInteger.
	self uFlags: (self uFlags bitOr: NIF_INFO)!

balloonTitle: aString 
	self szInfoTitle: aString.
	self uFlags: (self uFlags bitOr: NIF_INFO)!

dwInfoFlags: anObject
	"Set the receiver's dwInfoFlags field to the value of anObject."

	bytes dwordAtOffset: 484 put: anObject!

dwSize: anObject
	"Set the receiver's dwSize field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

dwState: anObject
	"Set the receiver's dwState field to the value of anObject."

	bytes dwordAtOffset: 152 put: anObject!

dwStateMask: anObject
	"Set the receiver's dwStateMask field to the value of anObject."

	bytes dwordAtOffset: 156 put: anObject!

hIcon: anObject
	"Set the receiver's hIcon field to the value of anObject."

	bytes dwordAtOffset: 20 put: anObject!

hWnd: anObject
	"Set the receiver's hWnd field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

icon: anIconOrHandle 
	"Set the receiver's hIcon field."

	self hIcon: anIconOrHandle asParameter.
	self uFlags: (self uFlags bitOr: NIF_ICON).
	!

message: anIntegerMessageNumber
	"Set the receiver's uCallbackMessage (message sent to window when mouse over
	the icon in the taskbar) field."

	self uCallbackMessage: anIntegerMessageNumber.
	self uFlags: (self uFlags bitOr: NIF_MESSAGE)!

szInfo
	"Answer the receiver's szInfo field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 160)!

szInfo: anObject
	"Set the receiver's szInfo field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (255 * 1).
	anObject replaceBytesOf: bytes from: 161 to: 160 + size startingAt: 1.
	bytes at: size+161 put: 0!

szInfoTitle
	"Answer the receiver's szInfoTitle field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 420)!

szInfoTitle: anObject
	"Set the receiver's szInfoTitle field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (63 * 1).
	anObject replaceBytesOf: bytes from: 421 to: 420 + size startingAt: 1.
	bytes at: size+421 put: 0!

szTip
	"Answer the receiver's szTip field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 24)!

szTip: anObject
	"Set the receiver's szTip field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (127 * 1).
	anObject replaceBytesOf: bytes from: 25 to: 24 + size startingAt: 1.
	bytes at: size+25 put: 0!

tipText: aString
	"Set the receiver's szTip (tip text) field."

	self szTip: aString.
	self uFlags: (self uFlags bitOr: NIF_TIP)!

uCallbackMessage: anObject
	"Set the receiver's uCallbackMessage field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject!

uFlags
	"Answer the receiver's uFlags field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

uFlags: anObject
	"Set the receiver's uFlags field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject!

uID: anObject
	"Set the receiver's uID field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

uTimeout: anObject
	"Set the receiver's uTimeout field to the value of anObject."

	bytes dwordAtOffset: 416 put: anObject!

uVersion: anObject
	"Set the receiver's uVersion field to the value of anObject."

	bytes dwordAtOffset: 416 put: anObject! !
!NOTIFYICONDATA2 categoriesFor: #balloonFlags:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #balloonMessage:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #balloonTimeout:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #balloonTitle:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #dwInfoFlags:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #dwSize:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #dwState:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #dwStateMask:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #hIcon:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #hWnd:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #icon:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #message:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #szInfo!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #szInfo:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #szInfoTitle!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #szInfoTitle:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #szTip!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #szTip:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #tipText:!accessing!public! !
!NOTIFYICONDATA2 categoriesFor: #uCallbackMessage:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #uFlags!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #uFlags:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #uID:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #uTimeout:!**compiled accessors**!public! !
!NOTIFYICONDATA2 categoriesFor: #uVersion:!**compiled accessors**!public! !

!NOTIFYICONDATA2 class methodsFor!

defineFields
	"Define the fields of the NOTIFYICONDATA2 'structure'.
		self compileDefinition

		struct {
			DWORD cbSize; 
			HWND hWnd; 
			UINT uID; 
			UINT uFlags; 
			UINT uCallbackMessage; 
			HICON hIcon; 
			char szTip[64]; 
		} NOTIFYICONDATA;
	"

	self
		defineField: #dwSize type: DWORDField writeOnly beOverride;
		defineField: #hWnd type: HANDLEField writeOnly;
		defineField: #uID type: DWORDField writeOnly;
		defineField: #uFlags type: DWORDField new;
		defineField: #uCallbackMessage type: DWORDField writeOnly;
		defineField: #hIcon type: HANDLEField writeOnly;
		defineField: #szTip type: (StringField length: 128);
		defineField: #dwState type: DWORDField writeOnly;
		defineField: #dwStateMask type: DWORDField writeOnly;
		defineField: #szInfo type: (StringField length: 256);
		defineField: #uTimeout type: DWORDField writeOnly offset: 416;
		defineField: #uVersion type: DWORDField writeOnly offset: 416;
		defineField: #szInfoTitle type: (StringField length: 64);
		defineField: #dwInfoFlags type: DWORDField writeOnly! !
!NOTIFYICONDATA2 class categoriesFor: #defineFields!initializing!public! !

