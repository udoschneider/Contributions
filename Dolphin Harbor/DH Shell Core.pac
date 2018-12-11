| package |
package := Package name: 'DH Shell Core'.
package paxVersion: 1;
	basicComment: 'Shell Core.

Copyright (c) Louis Sumberg and Steve Waring 2002.
	<lsumberg@mindspring.com>, <http://www.mindspring.com/~lsumberg/dolphin>
	<swaring@ozemail.com.au>, <http://www.dolphinharbor.org>
Public Domain Freeware.

This package provides access to objects in the Windows Shell namespace and services supplied by the Shell API.  ShellObject and its subclasses represent real (i.e., filesystem) directories and files, as well as virtual folders and files (e.g., My Computer and shortcuts).  The primary service provided by the core package is navigation through the Shell namespace.  (Additional services are provided by other Shell packages.)

The key Smalltalk objects in this package are instances of ShellFolder and ShellFile -- see ShellObject.  In general, ShellObjects speak to the Shell API via interaction with instances of IShellFolder and IEnumIDList, and with the singleton instances of ShellLibrary, ShellFolderLibrary and SchlwapiLibrary.  Other classes in the package, such as ITEMIDLIST and STRRET, represent Windows structures and are used in Windows API calls.'.

package basicPackageVersion: '5.1.2.11'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIAAAAAA=').

package classNames
	add: #IEnumIDList;
	add: #ShellFolder;
	add: #ShellFolderLibrary;
	add: #ShellObject;
	add: #SHFILEINFO;
	add: #SHFILEOPSTRUCT;
	add: #STRRET;
	add: #SystemImageManager;
	yourself.

package methodNames
	add: #IShellFolder -> #bindToObject:;
	add: #IShellFolder -> #childrenWithFlags:;
	add: #IShellFolder -> #collect:flags:;
	add: #IShellFolder -> #displayNameOf:flags:;
	add: #IShellFolder -> #do:flags:;
	add: #IShellFolder -> #enumObjectsWithFlags:;
	add: #IShellFolder -> #isFolder:;
	add: #ShellLibrary -> #SHFileOperation:;
	add: #ShellLibrary -> #SHGetDataFromIDList:pidl:nFormat:pv:cb:;
	add: #ShellLibrary -> #SHGetFileInfo:dwFileAttributes:psfi:cbFileInfo:uFlags:;
	add: #ShlwapiLibrary -> #strRetToBuf:pidl:pszBuf:cchBuf:;
	add: #String -> #addNull;
	add: 'WinImageList class' -> #systemIconSize:;
	add: 'WinImageList class' -> #systemLarge;
	add: 'WinImageList class' -> #systemSmall;
	yourself.

package globalNames
	add: #IShellFolderConstants;
	add: #ShellCSIDLConstants;
	add: #SHFILEOPConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\IDE\Base\Development System';
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Dialogs\Common\Dolphin Common Dialogs';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\ActiveX\COM\OLE COM';
	add: '..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

Object subclass: #ShellObject
	instanceVariableNames: 'idFull flags parseName'
	classVariableNames: 'IsDisplayEnumErrors IsHiddenIncluded'
	poolDictionaries: 'IShellFolderConstants ShellCSIDLConstants'
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #ShellFolderLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IEnumXXXX subclass: #IEnumIDList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OLEStructure subclass: #STRRET
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Win32Structure subclass: #SHFILEINFO
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'IShellFolderConstants'
	classInstanceVariableNames: ''!
Win32Structure subclass: #SHFILEOPSTRUCT
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'SHFILEOPConstants'
	classInstanceVariableNames: ''!
ImageManager subclass: #SystemImageManager
	instanceVariableNames: ''
	classVariableNames: 'Current'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShellObject subclass: #ShellFolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!IShellFolder methodsFor!

bindToObject: anITEMIDLIST
	"Answer an <IShellFolder> on the folder specified by anITEMIDLIST which is relative to the receiver."

	| answer |
	#shellAdded.
	answer := IShellFolder newPointer.
	self 
		BindToObject: anITEMIDLIST
		pbc: nil
		riid: self iid
		ppvOut: answer.
	^answer!

childrenWithFlags: anInteger
	"Answer a collection of single <ITEMIDLIST>s that are relative to the receiver, using anInteger 
	(one or more Windows SHCONTF flags) to determine which children."

	#shellAdded.
	^self collect: [:child | child] flags: anInteger!

collect: aOneArgumentBlock flags: anInteger
	"Evaluate the <monadicValuable> argument, aOneArgumentBlock, for each of the receiver's
	children, using anInteger (one or more Windows SHCONTF flags) to determine which children.
	Answer a collection of all evaluations."

	| answer |
	#shellAdded.
	answer := OrderedCollection new.
	self do: [:each | answer addLast: (aOneArgumentBlock value: each)] flags: anInteger.
	^answer!

displayNameOf: anITEMIDLIST flags: anInteger
	"Answer a <readableString> that is the receiver's name, using anITEMIDLIST which is relative to the 
	receiver, and anInteger (one or more Windows SHGDN flags) that specifies what type of name to return."

	| strRet result buffer |
	#shellAdded.
	strRet := STRRET new.
	self 
		GetDisplayNameOf: anITEMIDLIST
		uFlags: anInteger
		lpName: strRet.
	buffer := File pathBuffer.
	ShlwapiLibrary default 
		strRetToBuf: strRet yourAddress
		pidl: anITEMIDLIST
		pszBuf: buffer
		cchBuf: buffer size.
	^buffer trimNulls!

do: aOneArgumentBlock flags: anInteger
	"Evaluate the <monadicValuable> argument, aOneArgumentBlock, for each of the receiver's 
	children, using anInteger (one or more Windows SHCONTF flags) to determine which children."

	| ppenum pidlNext |
	#shellAdded.
	ppenum := self enumObjectsWithFlags: anInteger.
	[(pidlNext := ppenum nextAvailable) notNil] 
		whileTrue: [aOneArgumentBlock value: pidlNext]!

enumObjectsWithFlags: anInteger
	"Private - Answer an <IEnumIDList>, an enumeration object, on the receiver, using anInteger 
	(one or more Windows SHCONTF flags) to determine which children to include."

	| answer |
	#shellAdded.
	answer := IEnumIDList newPointer.
	self 
		EnumObjects: nil
		grfFlags: anInteger
		ppenumIDList: answer.
	^answer!

isFolder: anITEMIDLIST
	"Answer whether the shell object specified by anITEMIDLIST that is relative to the receiver is a folder."

	| folderConstant dword |
	#shellAdded.
	folderConstant := IShellFolderConstants at: #SFGAO_FOLDER.
	dword := DWORD fromInteger: folderConstant.
	self 
		GetAttributesOf: 1
		apidl: anITEMIDLIST
		rgfInOut: dword.
	^dword asSignedInteger allMask: folderConstant! !
!IShellFolder categoriesFor: #bindToObject:!operations!public! !
!IShellFolder categoriesFor: #childrenWithFlags:!accessing!public! !
!IShellFolder categoriesFor: #collect:flags:!enumerating!public! !
!IShellFolder categoriesFor: #displayNameOf:flags:!accessing!public! !
!IShellFolder categoriesFor: #do:flags:!enumerating!public! !
!IShellFolder categoriesFor: #enumObjectsWithFlags:!operations!private! !
!IShellFolder categoriesFor: #isFolder:!public!testing! !

!ShellLibrary methodsFor!

SHFileOperation: aSHFILEOPSTRUCT
	"Perform a file operation specified by the contents of aSHFILEOPSTRUCT.  Answer 0 if successful."

	<stdcall: dword SHFileOperation SHFILEOPSTRUCT*>
	^self invalidCall!

SHGetDataFromIDList: psf pidl: pidl nFormat: nFormat pv: pv cb: cb
	"Retrieve extended property data for a ShellObject.
		psf, the ShellObject's parent IShellFolder.
		pidl, an ITEMIDLIST relative to psf.
		nFormat, an Integer that specifies what kind of object pv is.
		pv, an <ExternalStructure> that will receive the data.
		cb, the size of pv.
	Answer 0 if successful."

	<stdcall: hresult SHGetDataFromIDListA lpvoid lpvoid sdword lpvoid sdword>
	^self invalidCall!

SHGetFileInfo: pszPath dwFileAttributes: dwFileAttributes psfi: psfi cbFileInfo: cbFileInfo uFlags: uFlags
	"Retrieve information about an object in the file system.
		pszPath, a String or ITEMIDLIST that contains the (absolute or relative) path and file name.
		dwFileAttributes, one or more Windows FILE_ATTRIBUTE flags.
		psfi, an SHFILEINFO object that receives the data.
		cbFileInfo, the size of psfi.
		uFlags, one or more Windows SHGFI flags that specify what type of information to retrieve.
	NB: If the uFlags parameter includes the SHGFI_PIDL flag, pszPath must be a fully qualified ITEMIDLIST.
	Answer a value whose meaning depends on the uFlags parameter."

	<stdcall: dword SHGetFileInfoA lpvoid dword SHFILEINFO* dword dword>
	^self invalidCall! !
!ShellLibrary categoriesFor: #SHFileOperation:!public!win32 functions-shell library! !
!ShellLibrary categoriesFor: #SHGetDataFromIDList:pidl:nFormat:pv:cb:!public!win32 functions-shell library! !
!ShellLibrary categoriesFor: #SHGetFileInfo:dwFileAttributes:psfi:cbFileInfo:uFlags:!public!win32 functions-shell library! !

!ShlwapiLibrary methodsFor!

strRetToBuf: pstr pidl: pidl pszBuf: buffer cchBuf: bufSize
	"Place the String representation of an STRRET in a buffer.
		pstr, a STRRET.
		pidl, an ITEMIDLIST.
		pszBuf, a String buffer to hold the display name.
		bufSize, the size of pszBuf.
	Answer 0 if successful, else an OLE error code."

	<stdcall: hresult StrRetToBufA STRRET* lpvoid lpstr dword>
	^self invalidCall! !
!ShlwapiLibrary categoriesFor: #strRetToBuf:pidl:pszBuf:cchBuf:!public!win32 functions-path! !

!String methodsFor!

addNull
	"Answer a copy of the receiver that has a null character appended to it."

	#shellAdded.
	^self , (String new: 1)! !
!String categoriesFor: #addNull!converting!public! !

!WinImageList class methodsFor!

systemIconSize: aConstant
	"Private - Answer a <WinImageList> for the system image list 
	From MSDN: Once you have a handle to a system image list, you can use the Image List API  
		to manipulate it like any other image list. Because system image lists are created on 
		a per-process basis, you should treat them as read-only objects. Writing to a system image
		list may overwrite or delete one of the system images, making it unavailable or incorrect
		for the remainder of the process. "

	| fileInfo ans |
	#shellAdded.
	fileInfo := SHFILEINFO new.
	ans := ShellLibrary default 
				SHGetFileInfo: 'c:\'
				dwFileAttributes: 0
				psfi: fileInfo
				cbFileInfo: fileInfo size
				uFlags: aConstant | (IShellFolderConstants at: #SHGFI_SYSICONINDEX).
	ans = 4294967295 ifTrue: [KernelLibrary default systemError].
	"we definately dont own this!!"
	^self fromHandle: ans!

systemLarge
	"Answer a <WinImageList> for the large system image list ."

	#shellAdded.
	^self systemIconSize: (IShellFolderConstants at: #SHGFI_LARGEICON)!

systemSmall
	"Answer a <WinImageList> for the small system image list "

	#shellAdded.
	^self systemIconSize: (IShellFolderConstants at: #SHGFI_SMALLICON)! !
!WinImageList class categoriesFor: #systemIconSize:!instance creation!private! !
!WinImageList class categoriesFor: #systemLarge!instance creation!public! !
!WinImageList class categoriesFor: #systemSmall!instance creation!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #IShellFolderConstants put: (PoolConstantsDictionary named: #IShellFolderConstants)!
IShellFolderConstants at: 'CMF_CANRENAME' put: 16r10!
IShellFolderConstants at: 'CMF_DEFAULTONLY' put: 16r1!
IShellFolderConstants at: 'CMF_EXPLORE' put: 16r4!
IShellFolderConstants at: 'CMF_INCLUDESTATIC' put: 16r40!
IShellFolderConstants at: 'CMF_NODEFAULT' put: 16r20!
IShellFolderConstants at: 'CMF_NORMAL' put: 16r0!
IShellFolderConstants at: 'CMF_NOVERBS' put: 16r8!
IShellFolderConstants at: 'CMF_RESERVED' put: -16r10000!
IShellFolderConstants at: 'CMF_VERBSONLY' put: 16r2!
IShellFolderConstants at: 'CMIC_MASK_ASYNCOK' put: 16r100000!
IShellFolderConstants at: 'CMIC_MASK_FLAG_NO_UI' put: 16r400!
IShellFolderConstants at: 'CMIC_MASK_HOTKEY' put: 16r20!
IShellFolderConstants at: 'CMIC_MASK_ICON' put: 16r10!
IShellFolderConstants at: 'CMIC_MASK_NO_CONSOLE' put: 16r8000!
IShellFolderConstants at: 'CMIC_MASK_UNICODE' put: 16r4000!
IShellFolderConstants at: 'FVM_DETAILS' put: 16r4!
IShellFolderConstants at: 'FVM_ICON' put: 16r1!
IShellFolderConstants at: 'FVM_LIST' put: 16r3!
IShellFolderConstants at: 'FVM_SMALLICON' put: 16r2!
IShellFolderConstants at: 'FWF_ABBREVIATEDNAMES' put: 16r2!
IShellFolderConstants at: 'FWF_ALIGNLEFT' put: 16r800!
IShellFolderConstants at: 'FWF_AUTOARRANGE' put: 16r1!
IShellFolderConstants at: 'FWF_BESTFITWINDOW' put: 16r10!
IShellFolderConstants at: 'FWF_DESKTOP' put: 16r20!
IShellFolderConstants at: 'FWF_NOCLIENTEDGE' put: 16r200!
IShellFolderConstants at: 'FWF_NOSCROLL' put: 16r400!
IShellFolderConstants at: 'FWF_NOSUBFOLDERS' put: 16r80!
IShellFolderConstants at: 'FWF_OWNERDATA' put: 16r8!
IShellFolderConstants at: 'FWF_SINGLECLICKACTIVATE' put: 16r8000!
IShellFolderConstants at: 'FWF_SINGLESEL' put: 16r40!
IShellFolderConstants at: 'FWF_SNAPTOGRID' put: 16r4!
IShellFolderConstants at: 'FWF_TRANSPARENT' put: 16r100!
IShellFolderConstants at: 'GCS_HELPTEXT' put: 16r1!
IShellFolderConstants at: 'GCS_UNICODE' put: 16r4!
IShellFolderConstants at: 'GCS_VALIDATE' put: 16r2!
IShellFolderConstants at: 'GCS_VERB' put: 16r0!
IShellFolderConstants at: 'SFGAO_CANCOPY' put: 16r1!
IShellFolderConstants at: 'SFGAO_CANDELETE' put: 16r20!
IShellFolderConstants at: 'SFGAO_CANLINK' put: 16r4!
IShellFolderConstants at: 'SFGAO_CANMOVE' put: 16r2!
IShellFolderConstants at: 'SFGAO_CANRENAME' put: 16r10!
IShellFolderConstants at: 'SFGAO_CAPABILITYMASK' put: 16r177!
IShellFolderConstants at: 'SFGAO_COMPRESSED' put: 16r4000000!
IShellFolderConstants at: 'SFGAO_CONTENTSMASK' put: -16r80000000!
IShellFolderConstants at: 'SFGAO_DISPLAYATTRMASK' put: 16rF0000!
IShellFolderConstants at: 'SFGAO_DROPTARGET' put: 16r100!
IShellFolderConstants at: 'SFGAO_FILESYSANCESTOR' put: 16r10000000!
IShellFolderConstants at: 'SFGAO_FILESYSTEM' put: 16r40000000!
IShellFolderConstants at: 'SFGAO_FOLDER' put: 16r20000000!
IShellFolderConstants at: 'SFGAO_GHOSTED' put: 16r80000!
IShellFolderConstants at: 'SFGAO_HASPROPSHEET' put: 16r40!
IShellFolderConstants at: 'SFGAO_HASSUBFOLDER' put: -16r80000000!
IShellFolderConstants at: 'SFGAO_LINK' put: 16r10000!
IShellFolderConstants at: 'SFGAO_READONLY' put: 16r40000!
IShellFolderConstants at: 'SFGAO_REMOVABLE' put: 16r2000000!
IShellFolderConstants at: 'SFGAO_SHARE' put: 16r20000!
IShellFolderConstants at: 'SFGAO_VALIDATE' put: 16r1000000!
IShellFolderConstants at: 'SHCONTF_FOLDERS' put: 16r20!
IShellFolderConstants at: 'SHCONTF_INCLUDEHIDDEN' put: 16r80!
IShellFolderConstants at: 'SHCONTF_NONFOLDERS' put: 16r40!
IShellFolderConstants at: 'SHGDN_FORADDRESSBAR' put: 16r4000!
IShellFolderConstants at: 'SHGDN_FORPARSING' put: 16r8000!
IShellFolderConstants at: 'SHGDN_INFOLDER' put: 16r1!
IShellFolderConstants at: 'SHGDN_NORMAL' put: 16r0!
IShellFolderConstants at: 'SHGFI_DISPLAYNAME' put: 16r200!
IShellFolderConstants at: 'SHGFI_ICON' put: 16r100!
IShellFolderConstants at: 'SHGFI_LARGEICON' put: 16r0!
IShellFolderConstants at: 'SHGFI_OPENICON' put: 16r2!
IShellFolderConstants at: 'SHGFI_PIDL' put: 16r8!
IShellFolderConstants at: 'SHGFI_SMALLICON' put: 16r1!
IShellFolderConstants at: 'SHGFI_SYSICONINDEX' put: 16r4000!
IShellFolderConstants at: 'SHGFI_TYPENAME' put: 16r400!
IShellFolderConstants at: 'STRRET_CSTR' put: 16r2!
IShellFolderConstants at: 'STRRET_OFFSET' put: 16r1!
IShellFolderConstants at: 'STRRET_WSTR' put: 16r0!
IShellFolderConstants at: 'SVGIO_ALLVIEW' put: 16r2!
IShellFolderConstants at: 'SVGIO_BACKGROUND' put: 16r0!
IShellFolderConstants at: 'SVGIO_SELECTION' put: 16r1!
IShellFolderConstants at: 'SVSI_DESELECT' put: 16r0!
IShellFolderConstants at: 'SVSI_DESELECTOTHERS' put: 16r4!
IShellFolderConstants at: 'SVSI_EDIT' put: 16r3!
IShellFolderConstants at: 'SVSI_ENSUREVISIBLE' put: 16r8!
IShellFolderConstants at: 'SVSI_FOCUSED' put: 16r10!
IShellFolderConstants at: 'SVSI_SELECT' put: 16r1!
IShellFolderConstants at: 'SVUIA_ACTIVATE_FOCUS' put: 16r2!
IShellFolderConstants at: 'SVUIA_ACTIVATE_NOFOCUS' put: 16r1!
IShellFolderConstants at: 'SVUIA_DEACTIVATE' put: 16r0!
IShellFolderConstants at: 'SVUIA_INPLACEACTIVATE' put: 16r3!
IShellFolderConstants shrink!

Smalltalk at: #ShellCSIDLConstants put: (PoolConstantsDictionary named: #ShellCSIDLConstants)!
ShellCSIDLConstants at: 'CSIDL_ADMINTOOLS' put: 16r30!
ShellCSIDLConstants at: 'CSIDL_ALTSTARTUP' put: 16r1D!
ShellCSIDLConstants at: 'CSIDL_APPDATA' put: 16r1A!
ShellCSIDLConstants at: 'CSIDL_BITBUCKET' put: 16rA!
ShellCSIDLConstants at: 'CSIDL_COMMON_ADMINTOOLS' put: 16r2F!
ShellCSIDLConstants at: 'CSIDL_COMMON_ALTSTARTUP' put: 16r1D!
ShellCSIDLConstants at: 'CSIDL_COMMON_APPDATA' put: 16r23!
ShellCSIDLConstants at: 'CSIDL_COMMON_DESKTOPDIRECTORY' put: 16r19!
ShellCSIDLConstants at: 'CSIDL_COMMON_DOCUMENTS' put: 16r2E!
ShellCSIDLConstants at: 'CSIDL_COMMON_FAVORITES' put: 16r1F!
ShellCSIDLConstants at: 'CSIDL_COMMON_PROGRAMS' put: 16r17!
ShellCSIDLConstants at: 'CSIDL_COMMON_STARTMENU' put: 16r16!
ShellCSIDLConstants at: 'CSIDL_COMMON_STARTUP' put: 16r18!
ShellCSIDLConstants at: 'CSIDL_COMMON_TEMPLATES' put: 16r2D!
ShellCSIDLConstants at: 'CSIDL_CONTROLS' put: 16r3!
ShellCSIDLConstants at: 'CSIDL_COOKIES' put: 16r21!
ShellCSIDLConstants at: 'CSIDL_DESKTOP' put: 16r0!
ShellCSIDLConstants at: 'CSIDL_DESKTOPDIRECTORY' put: 16r10!
ShellCSIDLConstants at: 'CSIDL_DRIVES' put: 16r11!
ShellCSIDLConstants at: 'CSIDL_FAVORITES' put: 16r6!
ShellCSIDLConstants at: 'CSIDL_FLAG_CREATE' put: 16r8000!
ShellCSIDLConstants at: 'CSIDL_FLAG_DONT_VERIFY' put: 16r4000!
ShellCSIDLConstants at: 'CSIDL_FONTS' put: 16r14!
ShellCSIDLConstants at: 'CSIDL_HISTORY' put: 16r22!
ShellCSIDLConstants at: 'CSIDL_INTERNET' put: 16r1!
ShellCSIDLConstants at: 'CSIDL_INTERNET_CACHE' put: 16r20!
ShellCSIDLConstants at: 'CSIDL_LOCAL_APPDATA' put: 16r1C!
ShellCSIDLConstants at: 'CSIDL_MYPICTURES' put: 16r27!
ShellCSIDLConstants at: 'CSIDL_NETHOOD' put: 16r13!
ShellCSIDLConstants at: 'CSIDL_NETWORK' put: 16r12!
ShellCSIDLConstants at: 'CSIDL_PERSONAL' put: 16r5!
ShellCSIDLConstants at: 'CSIDL_PRINTERS' put: 16r4!
ShellCSIDLConstants at: 'CSIDL_PRINTHOOD' put: 16r1B!
ShellCSIDLConstants at: 'CSIDL_PROFILE' put: 16r28!
ShellCSIDLConstants at: 'CSIDL_PROGRAM_FILES' put: 16r26!
ShellCSIDLConstants at: 'CSIDL_PROGRAM_FILES_COMMON' put: 16r2B!
ShellCSIDLConstants at: 'CSIDL_PROGRAM_FILES_COMMONX86' put: 16r2C!
ShellCSIDLConstants at: 'CSIDL_PROGRAM_FILESX86' put: 16r2A!
ShellCSIDLConstants at: 'CSIDL_PROGRAMS' put: 16r2!
ShellCSIDLConstants at: 'CSIDL_RECENT' put: 16r8!
ShellCSIDLConstants at: 'CSIDL_SENDTO' put: 16r9!
ShellCSIDLConstants at: 'CSIDL_STARTMENU' put: 16rB!
ShellCSIDLConstants at: 'CSIDL_STARTUP' put: 16r7!
ShellCSIDLConstants at: 'CSIDL_SYSTEM' put: 16r25!
ShellCSIDLConstants at: 'CSIDL_SYSTEMX86' put: 16r29!
ShellCSIDLConstants at: 'CSIDL_TEMPLATES' put: 16r15!
ShellCSIDLConstants at: 'CSIDL_WINDOWS' put: 16r24!
ShellCSIDLConstants shrink!

Smalltalk at: #SHFILEOPConstants put: (PoolConstantsDictionary named: #SHFILEOPConstants)!
SHFILEOPConstants at: 'FO_COPY' put: 16r2!
SHFILEOPConstants at: 'FO_DELETE' put: 16r3!
SHFILEOPConstants at: 'FO_MOVE' put: 16r1!
SHFILEOPConstants at: 'FO_RENAME' put: 16r4!
SHFILEOPConstants at: 'FOF_ALLOWUNDO' put: 16r40!
SHFILEOPConstants at: 'FOF_CONFIRMMOUSE' put: 16r2!
SHFILEOPConstants at: 'FOF_FILESONLY' put: 16r80!
SHFILEOPConstants at: 'FOF_MULTIDESTFILES' put: 16r1!
SHFILEOPConstants at: 'FOF_NO_CONNECTED_ELEMENTS' put: 16r2000!
SHFILEOPConstants at: 'FOF_NOCONFIRMATION' put: 16r10!
SHFILEOPConstants at: 'FOF_NOCONFIRMMKDIR' put: 16r200!
SHFILEOPConstants at: 'FOF_NOCOPYSECURITYATTRIBS' put: 16r800!
SHFILEOPConstants at: 'FOF_NOERRORUI' put: 16r400!
SHFILEOPConstants at: 'FOF_NORECURSION' put: 16r1000!
SHFILEOPConstants at: 'FOF_RENAMEONCOLLISION' put: 16r8!
SHFILEOPConstants at: 'FOF_SILENT' put: 16r4!
SHFILEOPConstants at: 'FOF_SIMPLEPROGRESS' put: 16r100!
SHFILEOPConstants at: 'FOF_WANTMAPPINGHANDLE' put: 16r20!
SHFILEOPConstants at: 'FOF_WANTNUKEWARNING' put: 16r4000!
SHFILEOPConstants at: 'S_OK' put: 16r0!
SHFILEOPConstants shrink!

"Classes"!

ShellObject guid: (GUID fromString: '{434AEAEB-5E2C-46AF-B256-A729432D25C9}')!
ShellObject comment: 'ShellObject represents and provides access to the Windows Shell Namespace, that is, to those objects that are managed by the Windows Shell.  The most numerous and familiar objects are the folders and files that reside on computer disk drives. However, there are also nonfile system, or virtual objects, such as network printers, other networked computers, Control Panel applications, and the Recycle Bin.

ShellObject provides methods to navigate through and enumerate objects in the Windows Shell namespace.  It does this by accessing and wrapping methods in other classes, particularly IShellFolder and ITEMIDLIST.

Instance Variables
	idFull			<ITEMIDLIST>
	flags			<Integer>
	parseName		<String>

Notes:
	-If Instances are loaded from a saved image, their idFull will be invalid. The parseName may be used to reconstruct the idFull, however this may not work in all circumstances.
	-The idFull is a full ITEMIDLIST (ie relative to the desktop) and is required for many of the ShellLibrary functions.
	-#idRelative and #parentInterface are required for many of the COM Functions and are constructed from the idFull.
	- The desktop is a special case. We use:
		An empty ITEMIDLIST for its idFull 
		A parseName of ''Desktop''
	-Win9X will not reliably do the roundTrip of parseName -> idl -> parseName. We were forced to construct our own parseNames for Win9X


Background:

The root of the namespace hierarchy is the desktop. Immediately below the root are several virtual folders such as My Computer and the Recycle Bin.  The file systems of the various disk drives can be seen to be subsets of the larger namespace hierarchy. The roots of these file systems are subfolders of the My Computer folder. My Computer also includes the roots of any mapped network drives. Other nodes in the tree, such as My Documents, are virtual folders.

Like the file system, the namespace includes two basic types of object: folders and files. Folder objects are the nodes of the tree; they are containers for file objects and other folders. File objects are the leaves of the tree; they are either normal disk files or virtual objects, such as printer links. Folders that are not part of the file system are sometimes referred to as virtual folders.

Thus, a ShellObject may be a:
	filesystem folder (e.g., c:\program files),
	filesystem file (e.g., c:\program files\desktop.ini),
	virtual folder (e.g., Desktop, My Computer, Recycle Bin),
	virtual file (e.g., Control Panel applications, printer links).

'!
!ShellObject categoriesForClass!Unclassified! !
!ShellObject methodsFor!

<= aShellObject
	"Answer whether the receiver is less than or equal to aShellObject."

	^self isWin9X 
		ifTrue: 
			[self isFolder = aShellObject isFolder 
				ifTrue: [self parseName <= aShellObject parseName]
				ifFalse: [self isFolder]]
		ifFalse: 
			[self = aShellObject or: 
					[(aShellObject isKindOf: ShellObject) and: 
							["(self parentInterface isSameCOMObject: aShellObject parentInterface) 
								and: ["

							self basicLessThanOrEquals: aShellObject	"]"]]]!

= aShellObject
	"Answer whether the receiver is equivalent to aShellObject."

	^self parseName = aShellObject parseName!

addParentsTo: aCollection
	| parent |
	(parent := self parent) notNil 
		ifTrue: 
			[aCollection addFirst: parent.
			parent addParentsTo: aCollection]
		ifFalse: [aCollection]!

allParents
	"Answer a collection of <ShellObjects> which are the parents of the receiver.
	The first item is the direct parent, the last is the desktop."

	| parents |
	parents := OrderedCollection new.
	self addParentsTo: parents.
	^parents!

basicDelete
	"Delete the receiver and try to send it to the Recycle Bin.  Answer whether the deletion was successful or not."

	^SHFILEOPSTRUCT delete: self displayParsingName!

basicEquals: aShellObject
	"Answer whether the Shell considers the receiver and aShellObject to be equal.
	The COM function can fail (for example Shell namespace extensions may fail to implement it)"

	^[(self desktopInterface 
		CompareIDs: 0
		pidl1: self idFull
		pidl2: aShellObject idFull) = 0] 
		on: Error
		do: [:e | false]!

basicIsFolder
	"Answer whether the receiver is a (virtual or filesystem) folder."

	^self hasAttribute: SFGAO_FOLDER!

basicLessThanOrEquals: aShellObject
	"Answer whether the Shell considers the receiver to be lessThan or equal to aShellObject.
	The COM function can fail (for example Shell namespace extensions may fail to implement it)"

	^[(self compareRelative: aShellObject) <= 0] on: Error
		do: [:e | self displayName <= aShellObject displayName]!

compareRelative: aShellObject
	"Answer an Integer that represents the order of aShellObject relative to the receiver."

	| answer hresult |
	hresult := self parentInterface 
				CompareIDs: 0
				pidl1: self idRelative
				pidl2: aShellObject idRelative.
	"extract the CODE field from the HRESULT"
	answer := (HRESULT fromUnsignedInteger: hresult) statusCode.
	"then cast the result to (short)"
	^answer >= 32768 ifTrue: [(65536 - answer) negated] ifFalse: [answer]!

delete
	"Delete the receiver and try to send it to the Recycle Bin.  Refresh the receiver if the shell object
	was deleted.  Answer whether the deletion was successful or not."

	self basicDelete 
		ifTrue: 
			[self refresh.
			^true].
	^false!

desktopInterface
	"Answer an <IShellFolder> that represents the Windows Desktop virtual folder."

	^self class desktopInterface!

displayName
	"Answer a <readableString> that is the receiver's name, relative to its parent folder.
	Ensure that we can answer something reasonable if the receiver is in the invalid state"

	^self isValid 
		ifTrue: [self parentInterface displayNameOf: self idRelative flags: SHGDN_INFOLDER]
		ifFalse: ['*invalid: ' , self parseName]!

displayNormalName
	"Answer a <readableString> that is the receiver's name, relative to the desktop, and used for generic display."

	^self parentInterface displayNameOf: self idRelative flags: SHGDN_NORMAL!

displayOn: aStream
	"Append to aStream a String representation of the receiver that a user would want to see."

	| name |
	name := [self displayName] on: Error do: [:e | 'error'].
	aStream display: name!

displayParsingName
	"Answer a <readableString> that is the receiver's full name.  For a filesystem object, this is 
	the full pathname of the directory or file.  For a virtual object, this may be a GUIID.  Note per MSDN, 
	there is no guarantee that the name returned will be successfully parsed by ShellObject>>fromPath:."

	^self desktopInterface displayNameOf: self idFull flags: SHGDN_FORPARSING!

fileSize
	"Answer the size, in bytes, of the object represented by the receiver, or nil if information is not available."

	^self getFindData ifNotNil: [:pv | pv fileSize]!

flags
	"Answer the receiver's flags that are used for display and error handling."

	^flags!

flags: anInteger
	"Set the receiver's flags that are used for display and error handling."

	flags := anInteger!

getFindData
	"Answer a <WIN32_FIND_DATA> for the receiver, or nil if the receiver is a virtual object."

	| pv |
	pv := WIN32_FIND_DATA new.
	
	[ShellLibrary default 
		SHGetDataFromIDList: self parentInterface
		pidl: self idRelative
		nFormat: 1
		pv: pv
		cb: pv size] 
			on: Error
			do: [:e | ^nil].
	^pv!

hasAttribute: anInteger
	"Answer whether the receiver has the attribute specified by anInteger."

	| dword |
	dword := DWORD fromInteger: anInteger.
	self parentInterface 
		GetAttributesOf: 1
		apidl: self idRelative
		rgfInOut: dword.
	^dword asSignedInteger allMask: anInteger!

hash
	"Answer the <integer> hash value for the receiver."

	^self parseName hash!

hasSubFolders
	"Answer whether the receiver contains any shell objects.  NB: true may be returned by the receiver even 
	if it does not contain subfolders. Returning true is recommended whenever a significant amount of 
	time is required to determine whether or not any subfolders exist. For example, all folders on 
	network drives answer true."

	^self hasAttribute: SFGAO_HASSUBFOLDER!

iconIndexWithFlags: anInteger
	"Answer an index into the system image list for the icon specified by anInteger."

	| fileInfo |
	fileInfo := SHFILEINFO new.
	^
	[ShellLibrary default 
		SHGetFileInfo: self idFull
		dwFileAttributes: nil
		psfi: fileInfo
		cbFileInfo: fileInfo size
		uFlags: anInteger.
	fileInfo iIcon] 
			on: Error
			do: [:e | 0]!

idFull
	"Private -  Answer the receiver's fully qualified <ITEMIDLIST>."

	^idFull!

idRelative
	"Private -  Answer an <ITEMIDLIST> that is relative to the receiver's parent folder."

	^self idFull idLast!

initialize
	super initialize.
	flags := 0!

initializeForFull: anITEMIDLIST
	"Initialize the receiver using the full ITEMIDLIST (i.e., relative to the Desktop).  From this,
	derive the receiver's parent id, relative id, and parentInterface."

	idFull := anITEMIDLIST.
	self setParseName!

isDesktop
	^false!

isDisplayEnumErrors
	"Answer whether a MessageBox should be shown when the receiver's folder can not be accessed."

	^flags anyMask: IsDisplayEnumErrors!

isDisplayEnumErrors: aBoolean
	"Set whether a MessageBox should be shown when the receiver's folder can not be accessed."

	flags := flags mask: IsDisplayEnumErrors set: aBoolean!

isFileSystem
	"Answer whether the receiver is part of the file system, that is, a file, directory, or root directory."

	^self hasAttribute: SFGAO_FILESYSTEM!

isFolder
	"Answer whether the receiver is a <ShellFolder>."

	^false!

isHiddenIncluded
	"Answer whether the receiver includes hidden objects when its contents are enumerated."

	^flags anyMask: IsHiddenIncluded!

isHiddenIncluded: aBoolean
	"Set whether the receiver will include hidden objects when its contents are enumerated."

	flags := flags mask: IsHiddenIncluded set: aBoolean!

isValid
	"Answer whether the receiver is in the valid state.
	a <ShellObject> will be invalid if it is saved into an image"

	^self idFull notNil!

isWin9X
	^self class isWin9X!

lastWriteTime
	"Answer a <FILETIME> for when the receiver was last written to, or nil."

	^self getFindData ifNotNil: [:pv | pv ftLastWriteTime copy]!

onStartup
	idFull := nil!

parent
	"Answer a <ShellFolder> that is the parent folder of the receiver, or nil if the receiver is the desktop folder."

	^self isDesktop ifTrue: [nil] ifFalse: [ShellFolder onIDFull: self idFull idParent]!

parentInterface
	"Private - Answer the <IShellFolder> that the receiver is relative to."

	| parentId |
	parentId := self idFull idParent.
	^parentId isDesktopID 
		ifTrue: [self desktopInterface]
		ifFalse: [self desktopInterface bindToObject: parentId]!

parseName
	^parseName!

parseName: aString
	parseName := aString!

parseNameWin9X
	^self isFileSystem 
		ifTrue: [self displayParsingName]
		ifFalse: [self parent parseName , '\' , self displayName]!

printOn: aStream
	"Append to aStream a String representation of the receiver that a developer would want to see."

	super printOn: aStream.
	aStream
		nextPut: $(;
		print: self displayName;
		nextPut: $)!

refresh
	"The receiver is in an invalid state if idFull is nil.
	Attempt to re-initialize it using the parseName.
	Leave the receiver in an invalid state if an error occurs"

	[self initializeForFull: (self class fromParseName: self parseName) idFull] on: Error
		do: [:e | idFull := nil]!

setParseName
	"We use a special parseName for the virtual desktop (because the Shell answers a parseName for the desktopFolder).
	We also build our own parseName for Win9X as it chokes on virtualFolders "

	parseName := idFull isDesktopID 
				ifTrue: ['Desktop']
				ifFalse: [self isWin9X ifTrue: [self parseNameWin9X] ifFalse: [self displayParsingName]]!

sysSmallIconIndex
	"Answer an index into the system image list for the receiver's small icon."

	^self iconIndexWithFlags: SHGFI_SMALLICON | SHGFI_PIDL | SHGFI_SYSICONINDEX!

sysSmallOpenIconIndex
	"Answer an index into the system image list for the receiver's small open icon."

	^self 
		iconIndexWithFlags: SHGFI_SMALLICON | SHGFI_OPENICON | SHGFI_PIDL | SHGFI_SYSICONINDEX!

typeName
	"Answer the <readableString> type (e.g., 'File Folder') of the receiver, or nil."

	| fileInfo |
	fileInfo := SHFILEINFO new.
	^
	[ShellLibrary default 
		SHGetFileInfo: self idFull
		dwFileAttributes: nil
		psfi: fileInfo
		cbFileInfo: fileInfo size
		uFlags: SHGFI_PIDL | SHGFI_TYPENAME.
	fileInfo szTypeName] 
			on: Error
			do: [:e | nil]! !
!ShellObject categoriesFor: #<=!comparing!public! !
!ShellObject categoriesFor: #=!comparing!public! !
!ShellObject categoriesFor: #addParentsTo:!accessing!private! !
!ShellObject categoriesFor: #allParents!accessing!public! !
!ShellObject categoriesFor: #basicDelete!deleting!private! !
!ShellObject categoriesFor: #basicEquals:!comparing!private! !
!ShellObject categoriesFor: #basicIsFolder!private!testing! !
!ShellObject categoriesFor: #basicLessThanOrEquals:!comparing!private! !
!ShellObject categoriesFor: #compareRelative:!comparing!private! !
!ShellObject categoriesFor: #delete!deleting!public! !
!ShellObject categoriesFor: #desktopInterface!accessing!private! !
!ShellObject categoriesFor: #displayName!displaying!public! !
!ShellObject categoriesFor: #displayNormalName!displaying!public! !
!ShellObject categoriesFor: #displayOn:!printing!public! !
!ShellObject categoriesFor: #displayParsingName!displaying!public! !
!ShellObject categoriesFor: #fileSize!accessing!public! !
!ShellObject categoriesFor: #flags!accessing!public! !
!ShellObject categoriesFor: #flags:!accessing!public! !
!ShellObject categoriesFor: #getFindData!accessing!private! !
!ShellObject categoriesFor: #hasAttribute:!private!testing! !
!ShellObject categoriesFor: #hash!comparing!public! !
!ShellObject categoriesFor: #hasSubFolders!public!testing! !
!ShellObject categoriesFor: #iconIndexWithFlags:!accessing!private! !
!ShellObject categoriesFor: #idFull!accessing!private! !
!ShellObject categoriesFor: #idRelative!accessing!private! !
!ShellObject categoriesFor: #initialize!initializing!private! !
!ShellObject categoriesFor: #initializeForFull:!initializing!private! !
!ShellObject categoriesFor: #isDesktop!public!testing! !
!ShellObject categoriesFor: #isDisplayEnumErrors!public!testing! !
!ShellObject categoriesFor: #isDisplayEnumErrors:!accessing!public! !
!ShellObject categoriesFor: #isFileSystem!public!testing! !
!ShellObject categoriesFor: #isFolder!public!testing! !
!ShellObject categoriesFor: #isHiddenIncluded!public!testing! !
!ShellObject categoriesFor: #isHiddenIncluded:!accessing!public! !
!ShellObject categoriesFor: #isValid!public!testing! !
!ShellObject categoriesFor: #isWin9X!private!testing! !
!ShellObject categoriesFor: #lastWriteTime!accessing!public! !
!ShellObject categoriesFor: #onStartup!event handling!private! !
!ShellObject categoriesFor: #parent!accessing!public! !
!ShellObject categoriesFor: #parentInterface!accessing!private! !
!ShellObject categoriesFor: #parseName!accessing!private! !
!ShellObject categoriesFor: #parseName:!accessing!private! !
!ShellObject categoriesFor: #parseNameWin9X!accessing!private! !
!ShellObject categoriesFor: #printOn:!printing!public! !
!ShellObject categoriesFor: #refresh!public!updating! !
!ShellObject categoriesFor: #setParseName!accessing!private! !
!ShellObject categoriesFor: #sysSmallIconIndex!accessing!public! !
!ShellObject categoriesFor: #sysSmallOpenIconIndex!accessing!public! !
!ShellObject categoriesFor: #typeName!accessing!public! !

!ShellObject class methodsFor!

desktopInterface
	"Private - Answer an <IShellFolder> that represents the Windows Desktop virtual folder."

	^ShellLibrary default getDesktopFolder!

fromParseName: aString
	"Using the <readableString> aString, answer an instance of the receiver."

	"The <ShellFolder> for the desktop is treated as a special case
		-It doesnt have a parentInterface.
		-We use 'Desktop' as the parseName to recognize it."

	^aString = 'Desktop' 
		ifTrue: [ShellFolder desktop]
		ifFalse: 
			[self isWin9X ifTrue: [self fromWin9XParseName: aString] ifFalse: [self fromPath: aString]]!

fromPath: aPathOrGUIIDString
	"Using the <readableString> aPathOrGUIIDString, answer an instance of the receiver."

	^self onIDFull: (self desktopInterface idlFromPath: aPathOrGUIIDString)!

fromVirtualName: aString
	"Private - Using the <readableString> aString, answer an instance of the receiver."

	| stream obj objName |
	aString = 'Desktop' ifTrue: [^ShellFolder desktop].
	stream := aString readStream.
	stream upTo: $\.
	obj := ShellFolder desktop.
	[(objName := stream upTo: $\) notEmpty] whileTrue: 
			[obj := obj subObjects detect: [:e | e displayName = objName] ifNone: [^self new parseName: aString]].
	^obj!

fromWin9XParseName: aString
	"Private - Using the <readableString> aString, answer an instance of the receiver."

	^(aString beginsWith: 'Desktop') 
		ifTrue: [self fromVirtualName: aString]
		ifFalse: [self fromPath: aString]!

initialize
	"
		self initialize
	"

	IsHiddenIncluded := 1.
	IsDisplayEnumErrors := 2.
	SessionManager current 
		when: #sessionStarted
		send: #onStartup
		to: self!

isWin9X
	^OSVERSIONINFO current isWin9X!

new
	^super new initialize!

onIDFull: anITEMIDLIST
	"Answer an instance of the receiver based on anITEMIDLIST, which is a full <ITEMIDLIST>."

	^(self new)
		initializeForFull: anITEMIDLIST;
		yourself!

onStartup
	"Private - Ensure all the receiver's subinstances are in their clean state on startup
	rather than attempting to use an old safe array hanging around from the sesssion 
	when the image was saved."

	self primAllSubinstances do: [:i | i onStartup]!

prompt
	"Prompt for and return a ShellObject, or nil if the user cancels."

	^FileOpenDialog showModal ifNotNil: [:e | self fromPath: e]!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	^(super publishedAspectsOfInstances)
		add: (Aspect string: #displayParsingName);
		add: (Aspect integer: #flags);
		add: (Aspect integer: #fileSize);
		add: (Aspect name: #getFindData);
		add: (Aspect boolean: #hasSubFolders);
		add: (Aspect boolean: #isDisplayEnumErrors);
		add: (Aspect boolean: #isFileSystem);
		add: (Aspect boolean: #isFolder);
		add: (Aspect boolean: #isHiddenIncluded);
		add: (Aspect name: #lastWriteTime);
		add: (Aspect name: #parseName);
		add: (Aspect string: #typeName);
		yourself!

refresh
	self primAllSubinstances do: [:e | e refresh]!

statusStringForAll: aCollection
	| total str |
	total := 0.
	aCollection 
		do: [:file | total := total + (file fileSize ifNil: [0] ifNotNil: [:fs | fs])].
	str := total > 1000000 
				ifTrue: [(total / 1024 / 1024) asInteger displayString , ' MB']
				ifFalse: [(total / 1024) asInteger displayString , ' KB'].
	^aCollection size displayString , ' object(s)  ' , str!

uninitialize
	SessionManager current removeEventsTriggeredFor: self! !
!ShellObject class categoriesFor: #desktopInterface!accessing!private! !
!ShellObject class categoriesFor: #fromParseName:!instance creation!public! !
!ShellObject class categoriesFor: #fromPath:!instance creation!public! !
!ShellObject class categoriesFor: #fromVirtualName:!instance creation win9x!private! !
!ShellObject class categoriesFor: #fromWin9XParseName:!instance creation win9x!private! !
!ShellObject class categoriesFor: #initialize!initializing!public! !
!ShellObject class categoriesFor: #isWin9X!private!testing! !
!ShellObject class categoriesFor: #new!instance creation!public! !
!ShellObject class categoriesFor: #onIDFull:!instance creation!public! !
!ShellObject class categoriesFor: #onStartup!event handling!private! !
!ShellObject class categoriesFor: #prompt!instance creation!public! !
!ShellObject class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !
!ShellObject class categoriesFor: #refresh!operations!public! !
!ShellObject class categoriesFor: #statusStringForAll:!helpers!public! !
!ShellObject class categoriesFor: #uninitialize!initializing!private! !

ShellFolderLibrary guid: (GUID fromString: '{88D49C99-24E2-4A4F-B0FC-A90CC1426C1B}')!
ShellFolderLibrary comment: ''!
!ShellFolderLibrary categoriesForClass!Unclassified! !
!ShellFolderLibrary methodsFor!

pathFromCSIDL: anInteger
	"Answer the full file path of the folder specified by anInteger, a Windows CSIDL , or nil if it doesn't exist."

	| buffer result |
	buffer := String new: File maxPath.
	result := self 
				SHGetFolderPath: nil
				nFolder: anInteger
				hToken: nil
				dwFlags: 0
				pszPath: buffer.
	^result = 0 ifTrue: [buffer trimNulls] ifFalse: [nil]!

SHGetFolderPath: hwndOwner nFolder: nFolder hToken: hToken dwFlags: dwFlags pszPath: pszPath
	"Takes the CSIDL of a folder and returns the pathname.

	HRESULT SHGetFolderPath(
		HWND hwndOwner,
		int nFolder,
		HANDLE hToken,
		DWORD dwFlags,
		LPTSTR pszPath
	);"

	<stdcall: hresult SHGetFolderPathA handle sdword handle dword lpstr>
	^self invalidCall
"Remarks from MSDN:
This function is a superset of SHGetSpecialFolderPath, included with earlier versions of the Shell. It is implemented in a redistributable DLL, SHFolder.dll, that also simulates many of the new Shell folders on older platforms such as Windows 95, Windows 98, and Windows NT 4.0. This DLL always calls the current platform's version of this function. If that fails, it will try to simulate the appropriate behavior. Only some CSIDLs are supported, including:
CSIDL_ADMINTOOLS = 16r30
CSIDL_APPDATA = 16r1A
CSIDL_COMMON_ADMINTOOLS = 16r2F
CSIDL_COMMON_APPDATA = 16r23
CSIDL_COMMON_DOCUMENTS = 16r2E
CSIDL_COOKIES = 16r21
CSIDL_FLAG_CREATE = 16r8000
CSIDL_HISTORY = 16r22
CSIDL_INTERNET_CACHE = 16r20
CSIDL_MYPICTURES = 16r27
CSIDL_PERSONAL = 16r5
CSIDL_PROGRAM_FILES = 16r26
CSIDL_PROGRAM_FILES_COMMON = 16r2B
CSIDL_SYSTEM = 16r25
CSIDL_WINDOWS = 16r24
"! !
!ShellFolderLibrary categoriesFor: #pathFromCSIDL:!public!win32 functions-shell library! !
!ShellFolderLibrary categoriesFor: #SHGetFolderPath:nFolder:hToken:dwFlags:pszPath:!public!win32 functions-shell library! !

!ShellFolderLibrary class methodsFor!

fileName
	"Answer the file name of the external library which the receiver represents."

	^'SHFolder'! !
!ShellFolderLibrary class categoriesFor: #fileName!constants!public! !

IEnumIDList guid: (IID fromString: '{000214F2-0000-0000-C000-000000000046}')!
IEnumIDList comment: 'IEnumIDList is a wrapper class for the IEnumIDList COM interface, and is used by IShellFolder (and thus by ShellObject).  An IEnumIDList instance is a pointer to a COM enumeration object.  It is created by IShellFolder>>enumObjectsWithFlags:, which is typically accessed through methods in ShellFolder.  Once an IEnumIDList is created, #nextAvailable can be used to enumerate the ITEMIDLISTs of the object.'!
!IEnumIDList categoriesForClass!Unclassified! !
!IEnumIDList methodsFor!

batchSize
	"Answer the number of elements to retrive per hit."

	^1!

nextAvailable
	"Answer a <ITEMIDLIST>.
	Next() answers a pointer to a block of WindowsShellMemory that we are responsible for deallocating.
	Create a Null instance of  WindowsShellMemory, set its value as the externalAddress and make it finalizable.
	This becomes the bytes of the ITEMIDLIST"

	| next |
	next := super nextAvailable.
	^next isNil 
		ifFalse: 
			[^(ITEMIDLIST basicNew)
				bytes: ((WindowsShellMemory new)
							value: next value asInteger;
							beFinalizable;
							yourself);
				initialize;
				yourself]!

nextAvailable: anInteger
	self shouldNotImplement! !
!IEnumIDList categoriesFor: #batchSize!constants!public! !
!IEnumIDList categoriesFor: #nextAvailable!accessing!public! !
!IEnumIDList categoriesFor: #nextAvailable:!accessing!public! !

!IEnumIDList class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'IShellFolderEx.IEnumIDList'
		IEnumIDList defineTemplate"

		"This method only present to prevent auto-generation from a type library"!

elementClass
	"Answer the class of element enumerated by the receiver."

	^LPVOID! !
!IEnumIDList class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!IEnumIDList class categoriesFor: #elementClass!constants!public! !

STRRET guid: (GUID fromString: '{17D058EB-7055-4F9A-BAE5-E776D4B06160}')!
STRRET comment: 'STRRET is an <ExternalStructure> that wraps the STRRET COM structure and is used by IShellFolder.  An instance of STRRET contains a string returned from the IShellFolder methods.  The uType aspect specifies what type of string is returned, e.g., a unicode string or a regular string.  The Windows function StrRetToBuf (in ShlwapiLibrary) may be used to easily extract the string, whatever its type is.   See IShellFolder>>displayNameOf:flags: for example usage.'!
!STRRET categoriesForClass!Unclassified! !
!STRRET methodsFor!

uType
	"#define STRRET_WSTR     0x0000          // Use STRRET.pOleStr
	#define STRRET_OFFSET   0x0001          // Use STRRET.uOffset to Ansi
	#define STRRET_CSTR     0x0002          // Use STRRET.cStr"

	^bytes sdwordAtOffset: 0!

uType: anObject
	"Set the receiver's uType field to the value of anObject."

	bytes sdwordAtOffset: 0 put: anObject! !
!STRRET categoriesFor: #uType!**compiled accessors**!public! !
!STRRET categoriesFor: #uType:!**compiled accessors**!public! !

!STRRET class methodsFor!

defineFields
	"Define the fields of the STRRET structure.
		STRRET compileDefinition
	
		typedef 		struct tagSTRRET {
			ESTRRET uType;
			BYTE cStr[260];
		} STRRET;
"

	self 
		defineField: #uType
		type: SDWORDField new
		offset: 0.
	self byteSize: (File maxPath + 4)! !
!STRRET class categoriesFor: #defineFields!initializing!public! !

SHFILEINFO guid: (GUID fromString: '{DD1D9617-7119-4617-90D5-7DD3B2148760}')!
SHFILEINFO comment: 'SHFILEINFO is an <ExternalStructure> that wraps the WIndows SHFILEINFO structure.  Its primary use is in calling ShellLibrary>>SHGetFileInfo: for icon and file information.'!
!SHFILEINFO categoriesForClass!Unclassified! !
!SHFILEINFO methodsFor!

dwAttributes
	"Answer the receiver's dwAttributes field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

hIcon
	"Answer the receiver's hIcon field as a Smalltalk object."

	^(bytes dwordAtOffset: 0) asExternalHandle!

hIcon: anObject
	"Set the receiver's hIcon field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

iIcon
	"Answer the receiver's iIcon field as a Smalltalk object."

	^(bytes sdwordAtOffset: 4)!

szDisplayName
	"Answer the receiver's szDisplayName field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 12)!

szTypeName
	"Answer the receiver's szTypeName field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 272)! !
!SHFILEINFO categoriesFor: #dwAttributes!**compiled accessors**!public! !
!SHFILEINFO categoriesFor: #hIcon!**compiled accessors**!public! !
!SHFILEINFO categoriesFor: #hIcon:!**compiled accessors**!public! !
!SHFILEINFO categoriesFor: #iIcon!**compiled accessors**!public! !
!SHFILEINFO categoriesFor: #szDisplayName!**compiled accessors**!public! !
!SHFILEINFO categoriesFor: #szTypeName!**compiled accessors**!public! !

!SHFILEINFO class methodsFor!

defineFields
	"
		SHFILEINFO compileDefinition

	typedef struct _SHFILEINFOA
	{
	        HICON       hIcon;                      // out: icon
	        int         iIcon;                      // out: icon index
	        DWORD       dwAttributes;               // out: SFGAO_ flags
	        CHAR        szDisplayName[MAX_PATH];    // out: display name (or path)
	        CHAR        szTypeName[80];             // out: type name
	} SHFILEINFOA;"

	self
		defineField: #hIcon type: HANDLEField new;
		defineField: #iIcon type: SDWORDField readOnly;
		defineField: #dwAttributes type: DWORDField readOnly;
		defineField: #szDisplayName type: (StringField length: File maxPath) beReadOnly;
		defineField: #szTypeName type: (StringField length: 80) beReadOnly!

displayName: aPathFileName
	| fileInfo |
	fileInfo := self new.
	(ShellLibrary default 
		SHGetFileInfo: aPathFileName
		dwFileAttributes: 0
		psfi: fileInfo
		cbFileInfo: fileInfo size
		uFlags: SHGFI_DISPLAYNAME) = 0 
		ifTrue: [ShellLibrary default systemError].
	^fileInfo szDisplayName!

icon: aPathFileName
	"Answer an <Icon>"

	^self 
		icon: aPathFileName
		flag: SHGFI_ICON
		attributes: nil	"Note: default is large icon"!

icon: aPathFileName flag: aConstant attributes: aConstant2
	"Answer an <Icon>"

	| fileInfo |
	fileInfo := self new.
	(ShellLibrary default 
		SHGetFileInfo: aPathFileName
		dwFileAttributes: aConstant2
		psfi: fileInfo
		cbFileInfo: fileInfo size
		uFlags: aConstant) = 0 
		ifTrue: [ShellLibrary default systemError].

	"MSDN: If SHGetFileInfo returns an icon handle in the hIcon member of the SHFILEINFO <../Structures/SHFILEINFO.htm> structure pointed to by psfi, you are responsible for freeing it with DestroyIcon when you no longer need it.
"
	^Icon fromOwnedHandle: fileInfo hIcon!

openIcon: aPathFileName
	"Answer an <Icon>"

	^self 
		icon: aPathFileName
		flag: SHGFI_ICON | SHGFI_OPENICON
		attributes: nil!

psuedoSmallIcon: aPathFileName
	"Answer an <Icon>
	MSDN:
		If the uFlags parameter includes the SHGFI_USEFILEATTRIBUTES flag, this parameter does not have to be a valid file name. The function proceeds as if the file exists with the specified name and with the file attributes passed in the dwFileAttributes parameter. This enables you to obtain information about a file type by passing just the extension for pszPath and passing FILE_ATTRIBUTE_NORMAL in dwFileAttributes."

	^self 
		icon: aPathFileName
		flag: SHGFI_ICON | SHGFI_SMALLICON | 16
		attributes: 128
	"SHGFI_USEFILEATTRIBUTES 	0x000000010 "
	"FILE_ATTRIBUTE_NORMAL               0x00000080 "!

smallIcon: aPathFileName
	"Answer an <Icon>"

	^self 
		icon: aPathFileName
		flag: SHGFI_ICON | SHGFI_SMALLICON
		attributes: nil!

smallOpenIcon: aPathFileName
	"Answer an <Icon>"

	^self 
		icon: aPathFileName
		flag: SHGFI_ICON | SHGFI_SMALLICON | SHGFI_OPENICON
		attributes: nil! !
!SHFILEINFO class categoriesFor: #defineFields!initializing!public! !
!SHFILEINFO class categoriesFor: #displayName:!helpers!public! !
!SHFILEINFO class categoriesFor: #icon:!helpers!public! !
!SHFILEINFO class categoriesFor: #icon:flag:attributes:!helpers!public! !
!SHFILEINFO class categoriesFor: #openIcon:!helpers!public! !
!SHFILEINFO class categoriesFor: #psuedoSmallIcon:!helpers!public! !
!SHFILEINFO class categoriesFor: #smallIcon:!helpers!public! !
!SHFILEINFO class categoriesFor: #smallOpenIcon:!helpers!public! !

SHFILEOPSTRUCT guid: (GUID fromString: '{8F9DF198-12CC-433D-A390-864F17B47915}')!
SHFILEOPSTRUCT comment: 'SHFILEOPSTRUCT is an <ExternalStructure> class that wraps the Windows SHFILEOPSTRUCT structure.  It can be used to move, copy, delete and rename files -- see class methods.  One benefit of this over methods in class File is that files to be deleted can be sent to the Recycle Bin.

From MSDN [NB: Edit this down]:

pFrom -- Address of a buffer to specify one or more source file names. These names must be fully qualified paths. Standard DOS wild cards, such as "*", are permitted in the file-name position. Although this member is declared as a null-terminated string, it is used as a buffer to hold multiple file names. Each file name must be terminated by a single NULL character. An additional NULL character must be appended to the end of the final name to indicate the end of pFrom. 

pTo -- Address of a buffer to contain the name of the destination file or directory. This parameter must be set to NULL if it is not used. Like pFrom, the pTo member is also a double-NULL terminated string and is handled in much the same way. However, pTo must meet the following specifications: 
- Wildcard characters are not supported. 
- Copy and Move operations can specify destination directories that do not exist and the system will attempt to create them. The system normally displays a dialog box to ask the user if they want to create the new directory. To suppress this dialog box and have the directories created silently, set the FOF_NOCONFIRMMKDIR flag in fFiles. 
For Copy and Move operations, the buffer can contain multiple destination file names if the fFlags member specifies FOF_MULTIDESTFILES. 
- Pack multiple names into the string in the same way as for pFrom. 
- Use only fully-qualified path names. Using relative pathnames will have unpredictable results.

fFlags -- Flags that control the file operation. This member can take a combination of the following flags:
- FOF_ALLOWUNDO  Preserve Undo information, if possible. If pFrom does not contain fully qualified path and file names, this flag is ignored.  
- FOF_FILESONLY  Perform the operation on files only if a wildcard file name (*.*) is specified.  
- FOF_MULTIDESTFILES  The pTo member specifies multiple destination files (one for each source file) rather than one directory where all source files are to be deposited.  
- FOF_NOCONFIRMATION  Respond with "Yes to All" for any dialog box that is displayed.  
- FOF_NOCONFIRMMKDIR  Do not confirm the creation of a new directory if the operation requires one to be created.  
- FOF_NO_CONNECTED_ELEMENTS Version 5.0. Do not move connected files as a group. Only move the specified files.  
FOF_NOCOPYSECURITYATTRIBS Version 4.71. Do not copy the security attributes of the file. 
- FOF_NOERRORUI  Do not display a user interface if an error occurs.  
- FOF_NORECURSION Only operate in the local directory. Don''t operate recursively into subdirectories. 
- FOF_RENAMEONCOLLISION  Give the file being operated on a new name in a move, copy, or rename operation if a file with the target name already exists.  
- FOF_SILENT  Do not display a progress dialog box.  
- FOF_SIMPLEPROGRESS  Display a progress dialog box but do not show the file names.  
- FOF_WANTMAPPINGHANDLE  If FOF_RENAMEONCOLLISION is specified and any files were renamed, assign a name mapping object containing their old and new names to the hNameMappings member. 
- FOF_WANTNUKEWARNING Version 5.0. Send a warning if a file is being destroyed during a delete operation rather than recycled. This flag partially overrides FOF_NOCONFIRMATION. 

fAnyOperationsAborted -- Value that receives TRUE if the user aborted any file operations before they were completed, or FALSE otherwise. 

hNameMappings -- A handle to a name mapping object containing the old and new names of the renamed files. This member is used only if the fFlags member includes the FOF_WANTMAPPINGHANDLE flag. Treat hNameMappings as a pointer to a structure whose first member is an INT value, followed by an array of SHNAMEMAPPING structures. The INT value will be set to the number of structures in the array. Each SHNAMEMAPPING structure will contain the old and new path name for one of the renamed files.  Note: The handle must be freed with SHFreeNameMappings.

lpszProgressTitle -- Address of a string to use as the title of a progress dialog box. This member is used only if fFlags includes the FOF_SIMPLEPROGRESS flag. 

Remarks -- If the pFrom or pTo members are unqualified names, the current directories are taken from the global current drive and directory settings as managed by the GetCurrentDirectory and SetCurrentDirectory functions.  If pFrom is set to a file name, deleting the file with FO_DELETE will not move it to the Recycle Bin, even if the FOF_ALLOWUNDO flag is set. You must use a full pathname.

See Also -- For a complete application that uses the SHFILEOPSTRUCT structure and explains how to set up the SHNAMEMAPPING structure, see the Knowledge Base Article Q151799 at http://support.microsoft.com/support/kb/articles/Q151/7/99.asp.

'!
!SHFILEOPSTRUCT categoriesForClass!Unclassified! !
!SHFILEOPSTRUCT methodsFor!

fAnyOperationsAborted
	"Answer the receiver's fAnyOperationsAborted field as a Smalltalk object."

	^(bytes sdwordAtOffset: 20)!

fAnyOperationsAborted: anObject
	"Set the receiver's fAnyOperationsAborted field to the value of anObject."

	bytes sdwordAtOffset: 20 put: anObject!

fFlags
	"Answer the receiver's fFlags field as a Smalltalk object."

	^(bytes swordAtOffset: 16)!

fFlags: anObject
	"Set the receiver's fFlags field to the value of anObject."

	bytes swordAtOffset: 16 put: anObject!

fileOperationFrom: fromString to: toString
	"Perform the shell operation specified by the receiver's aspects.  Answer true if successful."

	self
		pFrom: fromString yourAddress;
		pTo: toString yourAddress.
	^(ShellLibrary default SHFileOperation: self) == S_OK!

hNameMappings
	"Answer the receiver's hNameMappings field as a Smalltalk object."

	^(bytes sdwordAtOffset: 24)!

hNameMappings: anObject
	"Set the receiver's hNameMappings field to the value of anObject."

	bytes sdwordAtOffset: 24 put: anObject!

hwnd
	"Answer the receiver's hwnd field as a Smalltalk object."

	^(bytes sdwordAtOffset: 0)!

hwnd: anObject
	"Set the receiver's hwnd field to the value of anObject."

	bytes sdwordAtOffset: 0 put: anObject!

lpszProgressTitle
	"Answer the receiver's lpszProgressTitle field as a Smalltalk object."

	^(bytes sdwordAtOffset: 28)!

lpszProgressTitle: anObject
	"Set the receiver's lpszProgressTitle field to the value of anObject."

	bytes sdwordAtOffset: 28 put: anObject!

pFrom
	"Answer the receiver's pFrom field as a Smalltalk object."

	^(bytes sdwordAtOffset: 8)!

pFrom: anObject
	"Set the receiver's pFrom field to the value of anObject."

	bytes sdwordAtOffset: 8 put: anObject!

pTo
	"Answer the receiver's pTo field as a Smalltalk object."

	^(bytes sdwordAtOffset: 12)!

pTo: anObject
	"Set the receiver's pTo field to the value of anObject."

	bytes sdwordAtOffset: 12 put: anObject!

wFunc
	"Answer the receiver's wFunc field as a Smalltalk object."

	^(bytes sdwordAtOffset: 4)!

wFunc: anObject
	"Set the receiver's wFunc field to the value of anObject."

	bytes sdwordAtOffset: 4 put: anObject! !
!SHFILEOPSTRUCT categoriesFor: #fAnyOperationsAborted!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #fAnyOperationsAborted:!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #fFlags!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #fFlags:!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #fileOperationFrom:to:!operations!public! !
!SHFILEOPSTRUCT categoriesFor: #hNameMappings!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #hNameMappings:!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #hwnd!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #hwnd:!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #lpszProgressTitle!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #lpszProgressTitle:!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #pFrom!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #pFrom:!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #pTo!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #pTo:!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #wFunc!**compiled accessors**!public! !
!SHFILEOPSTRUCT categoriesFor: #wFunc:!**compiled accessors**!public! !

!SHFILEOPSTRUCT class methodsFor!

copy: fromString to: toString
	"Copy the file(s) specified by fromString to toString.  Answer true if successful."

	| instance |
	instance := (self new)
				wFunc: FO_COPY;
				fFlags: ((toString notNil and: [(toString occurrencesOf: Character null) > 1]) 
							ifTrue: [self defaultFlags | FOF_MULTIDESTFILES]
							ifFalse: [self defaultFlags]);
				yourself.
	^instance fileOperationFrom: fromString addNull to: toString addNull!

defaultFlags
	^FOF_ALLOWUNDO | FOF_NOCONFIRMATION | FOF_RENAMEONCOLLISION | FOF_WANTMAPPINGHANDLE!

defineFields
	"Define the fields of the SHFILEOPSTRUCT structure.
		SHFILEOPSTRUCT compileDefinition
	
		typedef 		struct tagSHFILEOPSTRUCT {
			long hwnd;
			int wFunc;
			long pFrom;
			long pTo;
			short fFlags;
			long fAnyOperationsAborted;
			long hNameMappings;
			long lpszProgressTitle;
		} SHFILEOPSTRUCT;
"

	self
		defineField: #hwnd type: SDWORDField new offset: 0;
		defineField: #wFunc type: SDWORDField new offset: 4;
		defineField: #pFrom type: SDWORDField new offset: 8;
		defineField: #pTo type: SDWORDField new offset: 12;
		defineField: #fFlags type: SWORDField new offset: 16;
		defineField: #fAnyOperationsAborted type: SDWORDField new offset: 20;
		defineField: #hNameMappings type: SDWORDField new offset: 24;
		defineField: #lpszProgressTitle type: SDWORDField new offset: 28.
	self byteSize: 32!

delete: fromString
	"Send to the Recycle Bin the file(s) specified by aString.  Answer true if successful."

	| instance |
	instance := (self new)
				wFunc: FO_DELETE;
				fFlags: self defaultFlags;
				yourself.
	^instance fileOperationFrom: fromString addNull to: nil!

deleteList: aCollectionOfStrings
	"Send to the Recycle Bin the file(s) specified by aCollectionOfStrings.  Answer true if successful."

	^self delete: (self packedString: aCollectionOfStrings)!

move: fromString to: toString
	"Move the file(s) specified by fromString to toString.  Answer true if successful."

	| instance |
	instance := (self new)
				wFunc: FO_MOVE;
				fFlags: ((toString notNil and: [(toString occurrencesOf: Character null) > 1]) 
							ifTrue: [self defaultFlags | FOF_MULTIDESTFILES]
							ifFalse: [self defaultFlags]);
				yourself.
	^instance fileOperationFrom: fromString addNull to: toString addNull!

packedString: aCollectionOfStrings
	"Answer a String that is the concatenation of all strings in aCollectionOfStrings, each string padded with a null."

	^(aCollectionOfStrings inject: String writeStream
		into: 
			[:stream :string | 
			stream
				nextPutAll: string addNull;
				yourself]) 
			contents!

packing
	"Answer the default packing for instances of the receiver."

	^1!

rename: fromString to: toString
	"Rename the file(s) specified by fromString to toString.  Answer true if successful."

	| instance |
	instance := (self new)
				wFunc: FO_RENAME;
				fFlags: self defaultFlags;
				yourself.
	^instance fileOperationFrom: fromString addNull to: toString addNull! !
!SHFILEOPSTRUCT class categoriesFor: #copy:to:!operations!public! !
!SHFILEOPSTRUCT class categoriesFor: #defaultFlags!constants!public! !
!SHFILEOPSTRUCT class categoriesFor: #defineFields!**auto generated**!initializing!public! !
!SHFILEOPSTRUCT class categoriesFor: #delete:!operations!public! !
!SHFILEOPSTRUCT class categoriesFor: #deleteList:!operations!public! !
!SHFILEOPSTRUCT class categoriesFor: #move:to:!operations!public! !
!SHFILEOPSTRUCT class categoriesFor: #packedString:!helpers!public! !
!SHFILEOPSTRUCT class categoriesFor: #packing!constants!public! !
!SHFILEOPSTRUCT class categoriesFor: #rename:to:!operations!public! !

SystemImageManager guid: (GUID fromString: '{0330BE77-AEDA-4EE3-95E1-A89987C919D6}')!
SystemImageManager comment: 'The singleton instance of SystemImageManager ...'!
!SystemImageManager categoriesForClass!Unclassified! !
!SystemImageManager methodsFor!

addImage: anImage maskcolor: aColor
	| largeIndex smallIndex |
	imageLists isEmpty ifTrue: [self buildImageListWithExtent: anImage extent].
	largeIndex := (anImage addToImageList: self largeImageList mask: aColor) + 1.
	smallIndex := (anImage addToImageList: self smallImageList mask: aColor) + 1.
	self assert: [largeIndex = smallIndex].
	^images at: anImage ifAbsentPut: [largeIndex]!

assertExtentSupported: aPoint
	((Array with: Icon largeExtent with: Icon smallExtent) includes: aPoint) 
		ifFalse: [self error: 'Icon Extent not supported']!

backcolor: aColor
	"Do Nothing"

	!

buildImageListWithExtent: aPoint
	| large small |
	self assertExtentSupported: aPoint.
	large := imageLists at: Icon largeExtent put: WinImageList systemLarge.
	small := imageLists at: Icon smallExtent put: WinImageList systemSmall.
	"If this assert fails, adding images to these lists will fail."
	self assert: [large getImageCount = small getImageCount].
	^imageLists at: aPoint!

imageAt: anInteger 
	Error notYetImplemented!

imageAt: anInteger ifAbsent: exceptionHandler
	Error notYetImplemented!

imageLists
	"Private - UnitTest helper method"

	^imageLists!

imageListWithExtent: aPoint
	self assertExtentSupported: aPoint.
	^super imageListWithExtent: aPoint!

initialize
	"Private - Initialize the receiver."

	super initialize.
	maskcolor := Color none.!

isExtentLarge: aPoint
	^aPoint = Icon largeExtent!

largeImageList
	^imageLists at: Icon largeExtent!

maskcolor: aColorOrNil
	"Do Nothing"

	!

newImageListWithExtent: aPoint
	self shouldNotImplement!

smallImageList
	^imageLists at: Icon smallExtent! !
!SystemImageManager categoriesFor: #addImage:maskcolor:!adding!public! !
!SystemImageManager categoriesFor: #assertExtentSupported:!asserting!private! !
!SystemImageManager categoriesFor: #backcolor:!accessing!public! !
!SystemImageManager categoriesFor: #buildImageListWithExtent:!helpers!private! !
!SystemImageManager categoriesFor: #imageAt:!accessing!public! !
!SystemImageManager categoriesFor: #imageAt:ifAbsent:!accessing!public! !
!SystemImageManager categoriesFor: #imageLists!private!test accessing! !
!SystemImageManager categoriesFor: #imageListWithExtent:!accessing!public! !
!SystemImageManager categoriesFor: #initialize!initializing!private! !
!SystemImageManager categoriesFor: #isExtentLarge:!helpers!private!testing! !
!SystemImageManager categoriesFor: #largeImageList!accessing!public! !
!SystemImageManager categoriesFor: #maskcolor:!accessing!public! !
!SystemImageManager categoriesFor: #newImageListWithExtent:!helpers!private! !
!SystemImageManager categoriesFor: #smallImageList!accessing!public! !

!SystemImageManager class methodsFor!

current
	"Answer the singleton instance of the receiver"

	Current isNil ifTrue: [Current := super new].
	^Current!

new
	"Private - The receiver is a singleton class"

	^self shouldNotImplement!

reset
	"Reset the receiver. Kill off the current singleton instance"

	Current := nil!

uninitialize
	"Private - Uninitialize the receiver as it is about to be removed from the system."

	Current := nil.! !
!SystemImageManager class categoriesFor: #current!instance creation!public! !
!SystemImageManager class categoriesFor: #new!instance creation!private! !
!SystemImageManager class categoriesFor: #reset!operations!public! !
!SystemImageManager class categoriesFor: #uninitialize!class hierarchy-removing!private! !

ShellFolder guid: (GUID fromString: '{59033BCB-C7D4-4563-A9F0-C2E86830CB61}')!
ShellFolder comment: 'ShellFolder represents a ShellObject that is a folder, i.e., a filesystem directory or a virtual folder that may contain other shell objects.'!
!ShellFolder categoriesForClass!Unclassified! !
!ShellFolder methodsFor!

allSubFolders
	"Answer a collection of <ShellFolder>, in and below the receiver."

	| aCollection |
	aCollection := OrderedCollection new.
	self subFolders do: 
			[:each | 
			aCollection add: each.
			each hasSubFolders ifTrue: [aCollection addAll: each allSubFolders]].
	^aCollection!

allSubObjects
	"Answer a collection of <ShellObject>, in and below the receiver."

	| aCollection |
	self isDesktop ifTrue: [^self error: 'This is gonna take way too long!!!!'].
	aCollection := OrderedCollection new.
	self subObjects do: 
			[:each | 
			aCollection add: each.
			each isFolder ifTrue: [aCollection addAll: each allSubObjects]].
	^aCollection!

childrenWithFlags: anInteger
	"Private - Answer a collection of <ITEMIDLIST> that are relative to the receiver.  NB:  The receiver 
	has aspects that filter what objects to include and what user notification to take on errors."

	^
	[self folderInterface 
		childrenWithFlags: anInteger | (self isHiddenIncluded ifTrue: [SHCONTF_INCLUDEHIDDEN] ifFalse: [0])] 
			on: Error
			do: 
				[:e | 
				self isDisplayEnumErrors ifTrue: [MessageBox errorMsg: self displayName , ' is not accessible'].
				OrderedCollection new]!

folderInterface
	"Private - Answer an <IShellFolder> that represents the receiver."

	^idFull isDesktopID 
		ifTrue: [self parentInterface]
		ifFalse: [self parentInterface bindToObject: self idRelative]!

initializeForDesktop
	"Initialize the receiver to represent the Desktop folder."

	self initializeForFull: ITEMIDLIST newBuffer!

isDesktop
	^idFull isDesktopID 
!

isFolder
	^true!

siblingOrParent
	"Answer a ShellFolder that is the receiver's next sibling, or previous sibling if 
	the receiver is the last, or the receiver's parent if the receiver has no siblings."

	| parent siblings pos |
	self isDesktop ifTrue: [^self].
	parent := self parent.
	siblings := parent subFolders asSortedCollection: [:a :b | a <= b].
	(siblings includes: self) ifTrue: [siblings remove: self].
	siblings isEmpty ifTrue: [^parent].
	siblings last displayName < self displayName ifTrue: [^siblings last].
	^siblings detect: [:e | e displayName > self displayName]!

sizeOfFiles
	"Answer the size, in bytes, of the files contained by the receiver."

	^self subFiles inject: 0
		into: [:sum :e |  sum + (e fileSize isNil ifTrue: [0] ifFalse: [e fileSize])]!

subFileNames
	"Answer a collection of <readableString>, for all files (non-folders) that the receiver contains."

	^self subFiles collect: [:each | each displayName]!

subFiles
	"Answer a collection of <ShellFile>, for all files (non-folders) that the receiver contains."

	^self subObjectsFrom: (self childrenWithFlags: SHCONTF_NONFOLDERS)!

subFolderNames
	"Answer a collection of <readableString>, for all folders that the receiver contains."

	^self subFolders collect: [:each | each displayName]!

subFolders
	"Answer a collection of <ShellFolder>, for all folders that the receiver contains."

	^self subObjectsFrom: (self childrenWithFlags: SHCONTF_FOLDERS)!

subObjectNames
	"Answer a collection of <readableString>, for all objects that the receiver contains."

	^self subObjects collect: [:each | each displayName]!

subObjects
	"Answer a collection of <ShellObject>, for all objects that the receiver contains."

	^self subObjectsFrom: (self childrenWithFlags: SHCONTF_FOLDERS | SHCONTF_NONFOLDERS)!

subObjectsFrom: aCollectionOfIDLs
	"Answer a collection of <ShellObject> based on aCollectionOfIDLs, where each single ITEMIDLIST 
	is relative to the receiver's <IShellFolder> folderInterface.  Copy the receiver's flags into each sub object"

	| baseInterface |
	baseInterface := self folderInterface.
	^aCollectionOfIDLs collect: 
			[:itemID | 
			(((baseInterface isFolder: itemID) ifTrue: [ShellFolder] ifFalse: [ShellObject]) 
				onIDFull: (idFull append: itemID)) flags: self flags]! !
!ShellFolder categoriesFor: #allSubFolders!accessing!public! !
!ShellFolder categoriesFor: #allSubObjects!accessing!public! !
!ShellFolder categoriesFor: #childrenWithFlags:!accessing!private! !
!ShellFolder categoriesFor: #folderInterface!accessing!private! !
!ShellFolder categoriesFor: #initializeForDesktop!initializing!public! !
!ShellFolder categoriesFor: #isDesktop!public!testing! !
!ShellFolder categoriesFor: #isFolder!public!testing! !
!ShellFolder categoriesFor: #siblingOrParent!accessing!public! !
!ShellFolder categoriesFor: #sizeOfFiles!accessing!public! !
!ShellFolder categoriesFor: #subFileNames!accessing!public! !
!ShellFolder categoriesFor: #subFiles!accessing!public! !
!ShellFolder categoriesFor: #subFolderNames!accessing!public! !
!ShellFolder categoriesFor: #subFolders!accessing!public! !
!ShellFolder categoriesFor: #subObjectNames!accessing!public! !
!ShellFolder categoriesFor: #subObjects!accessing!public! !
!ShellFolder categoriesFor: #subObjectsFrom:!accessing!public! !

!ShellFolder class methodsFor!

cDrive
	"Answer an instance of the receiver that represents the filesystem c drive, i.e., 'c:\'."

	^self fromPath: 'c:\'!

controlPanel
	"Answer an instance of the receiver that represents the Windows Control Panel virtual folder"

	^self isWin9X 
		ifTrue: [self myComputer subFolders detect: [:e | e displayName = 'Control Panel']]
		ifFalse: 
			[self 
				fromPath: '::{20D04FE0-3AEA-1069-A2D8-08002B30309D}\::{21EC2020-3AEA-1069-A2DD-08002B30309D}']!

desktop
	"Answer an instance of the receiver that represents the Windows Desktop virtual folder.
	The <ShellFolder> for the desktop is treated as a special case
		-It doesnt have a parentInterface.
		-We use 'Desktop' as the parseName to recognize it."

	^self new initializeForDesktop!

desktopDirectory
	"Answer an instance of the receiver that represents the Windows Desktop filesystem folder,
	e.g., 'C:\WINDOWS\Desktop'."

	^self forCSIDL: CSIDL_DESKTOPDIRECTORY!

favorites
	"Answer an instance of the receiver that represents the Windows Favorites filesystem folder."

	^self forCSIDL: CSIDL_FAVORITES!

forCSIDL: anInteger
	"Answer an instance of the receiver that represents the folder specified by anInteger, a Windows CSIDL value."

	^self fromPath: (ShellFolderLibrary default pathFromCSIDL: anInteger)!

myComputer
	"Answer an instance of the receiver that represents the Windows My Computer virtual folder."

	^self isWin9X 
		ifTrue: [self desktop subFolders detect: [:e | e displayName = 'My Computer']]
		ifFalse: [self fromPath: '::{20D04FE0-3AEA-1069-A2D8-08002B30309D}']!

myDocuments
	"Answer an instance of the receiver that represents the Windows My Documents filesystem folder.
	Note: MS recommends using the GUIID.  In the case of multiple users on one pc,
	this returns the My Documents folder for the current user."

	| obj |
	obj := self fromPath: '::{450d8fba-ad25-11d0-98a8-0800361b1103}'.
	self isWin9X ifTrue: [obj := self fromPath: obj displayParsingName].
	^obj!

programFiles
	"Answer an instance of the receiver that represents the WIndows Program Files filesystem folder."

	^self forCSIDL: CSIDL_PROGRAM_FILES!

prompt
	"Prompt for and return a ShellFolder, or nil if the user cancels."

	^BrowseFolderDialog showModal ifNotNil: [:e | self fromPath: e]!

recent
	"Answer an instance of the receiver that represents the Windows Recent filesystem folder."

	^self forCSIDL: CSIDL_RECENT!

windows
	"Answer an instance of the receiver that represents the WIndows Program Files filesystem folder."

	^self forCSIDL: CSIDL_WINDOWS! !
!ShellFolder class categoriesFor: #cDrive!public!special folders! !
!ShellFolder class categoriesFor: #controlPanel!public!special folders! !
!ShellFolder class categoriesFor: #desktop!instance creation!public! !
!ShellFolder class categoriesFor: #desktopDirectory!public!special folders! !
!ShellFolder class categoriesFor: #favorites!public!special folders! !
!ShellFolder class categoriesFor: #forCSIDL:!instance creation!public! !
!ShellFolder class categoriesFor: #myComputer!public!special folders! !
!ShellFolder class categoriesFor: #myDocuments!instance creation!public! !
!ShellFolder class categoriesFor: #programFiles!public!special folders! !
!ShellFolder class categoriesFor: #prompt!instance creation!public! !
!ShellFolder class categoriesFor: #recent!public!special folders! !
!ShellFolder class categoriesFor: #windows!public!special folders! !

"Binary Globals"!

