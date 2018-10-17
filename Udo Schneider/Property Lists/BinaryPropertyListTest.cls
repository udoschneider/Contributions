"Filed out from Dolphin Smalltalk 7"!

PropertyListTest subclass: #BinaryPropertyListTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BinaryPropertyListTest guid: (GUID fromString: '{da5ab207-198d-4e5d-be38-310ad3111c9d}')!
BinaryPropertyListTest comment: 'SUnitBrowser openOnTestCase: BinaryPropertyListTest'!
!BinaryPropertyListTest categoriesForClass!Unclassified! !
!BinaryPropertyListTest class methodsFor!

filesAndMethods
	^##((Dictionary new)
		at: 'array.bplist' put: 'plistArray';
		at: 'array_nested.bplist' put: 'plistNestedArray';
		at: 'data.bplist' put: 'plistData';
		at: 'date.bplist' put: 'plistDate';
		at: 'dictionary.bplist' put: 'plistDictionary';
		at: 'dictionary_nested.bplist' put: 'plistNestedDictionary';
		at: 'false.bplist' put: 'plistFalse';
		at: 'integer.bplist' put: 'plistInteger';
		at: 'nplist.bplist' put: 'plistNotAPlist';
		at: 'plist_v2.bplist' put: 'plistWrongVersion';
		at: 'real.bplist' put: 'plistReal';
		at: 'string.bplist' put: 'plistString';
		at: 'string_dquotes.bplist' put: 'plistStringWithDoubleQuotes';
		at: 'string_squotes.bplist' put: 'plistStringWithSingleQuotes';
		at: 'true.bplist' put: 'plistTrue';
		yourself)!

plistArray
	"
		self createPlistMethod: 'plistArray' fromFile: 'array.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDCnAQIDBAUGB18QMlRoZSBxdWljayBicm93biBmb3gganVtcHMgb3ZlciB0aGUgbGF6
eSBkb2cncyBiYWNrEQMvI0AJIfr8iwB6CQgzQaEptKoAAABIASNFZ4mrze8IEEVIUVJTXAAAAAAA
AAEBAAAAAAAAAAgAAAAAAAAAAAAAAAAAAABl')).!

plistData
	"
		self createPlistMethod: 'plistData' fromFile: 'data.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDBIASNFZ4mrze8IAAAAAAAAAQEAAAAAAAAAAQAAAAAAAAAAAAAAAAAAABE=')).!

plistDate
	"
		self createPlistMethod: 'plistDate' fromFile: 'date.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDAzQaEptKoAAAAIAAAAAAAAAQEAAAAAAAAAAQAAAAAAAAAAAAAAAAAAABE=')).!

plistDictionary
	"
		self createPlistMethod: 'plistDictionary' fromFile: 'dictionary.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDDXAQIDBAUGBwgJCgsMDQ5WbnVtYmVyVWZhbHNlVHJlYWxWc3RyaW5nVHRydWVUZGF0
ZVRkYXRhEQMvCCNACSH6/IsAel8QMlRoZSBxdWljayBicm93biBmb3gganVtcHMgb3ZlciB0aGUg
bGF6eSBkb2cncyBiYWNrCTNBoSm0qgAAAEgBI0VniavN7wgXHiQpMDU6P0JDTIGCiwAAAAAAAAEB
AAAAAAAAAA8AAAAAAAAAAAAAAAAAAACU')).!

plistFalse
	"
		self createPlistMethod: 'plistFalse' fromFile: 'false.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDAICAAAAAAAAAEBAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAJ')).!

plistInteger
	"
		self createPlistMethod: 'plistInteger' fromFile: 'integer.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDARAy8IAAAAAAAAAQEAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAs=')).!

plistNestedArray
	"
		self createPlistMethod: 'plistNestedArray' fromFile: 'array_nested.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDCoAQIDBAUGBwhfEDJUaGUgcXVpY2sgYnJvd24gZm94IGp1bXBzIG92ZXIgdGhlIGxh
enkgZG9nJ3MgYmFjaxEDLyNACSH6/IsAegkIM0GhKbSqAAAASAEjRWeJq83v2AkKCwwNDg8QAgUD
AQQTBgdWbnVtYmVyVWZhbHNlVHJlYWxWc3RyaW5nVHRydWVVYXJyYXlUZGF0ZVRkYXRhCAmnAQID
BAUGBwkICBFGSVJTVF1md36EiZCVm6ClpqevsAAAAAAAAAEBAAAAAAAAABYAAAAAAAAAAAAAAAAA
AACx')).!

plistNestedDictionary
	"
		self createPlistMethod: 'plistNestedDictionary' fromFile: 'dictionary_nested.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDDYAQIDBAUGBwgJCgsMDQ4RElZudW1iZXJVZmFsc2VUcmVhbFZzdHJpbmdUdHJ1ZVVh
cnJheVRkYXRlVGRhdGERAy8II0AJIfr8iwB6XxAyVGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBv
dmVyIHRoZSBsYXp5IGRvZydzIGJhY2sJqAwJCw0KERITCQgzQaEptKoAAABIASNFZ4mrze/XAQID
BAUHCAkKCwwNERIICQgZICYrMjc9QkdKS1SJipOUlZ6ntrcAAAAAAAABAQAAAAAAAAAWAAAAAAAA
AAAAAAAAAAAAuA==')).!

plistNotAPlist
	"
		self createPlistMethod: 'plistNotAPlist' fromFile: 'nplist.bplist'
	"
	^##((ByteArray fromBase64String: 'cHBsaXN0MDAjQAkh+vyLAHoIAAAAAAAAAQEAAAAAAAAAAQAAAAAAAAAAAAAAAAAAABE=')).!

plistReal
	"
		self createPlistMethod: 'plistReal' fromFile: 'real.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDAjQAkh+vyLAHoIAAAAAAAAAQEAAAAAAAAAAQAAAAAAAAAAAAAAAAAAABE=')).!

plistString
	"
		self createPlistMethod: 'plistString' fromFile: 'string.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDBfEDJUaGUgcXVpY2sgYnJvd24gZm94IGp1bXBzIG92ZXIgdGhlIGxhenkgZG9nJ3Mg
YmFjawgAAAAAAAABAQAAAAAAAAABAAAAAAAAAAAAAAAAAAAAPQ==')).!

plistStringWithDoubleQuotes
	"
		self createPlistMethod: 'plistStringWithDoubleQuotes' fromFile: 'string_dquotes.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDBfEDRUaGUgInF1aWNrIGJyb3duIiBmb3gganVtcHMgb3ZlciB0aGUgbGF6eSBkb2cn
cyBiYWNrCAAAAAAAAAEBAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAA/')).!

plistStringWithSingleQuotes
	"
		self createPlistMethod: 'plistStringWithSingleQuotes' fromFile: 'string_squotes.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDBfEDRUaGUgJ3F1aWNrIGJyb3duJyBmb3gganVtcHMgb3ZlciB0aGUgbGF6eSBkb2cn
cyBiYWNrCAAAAAAAAAEBAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAA/')).!

plistTrue
	"
		self createPlistMethod: 'plistTrue' fromFile: 'true.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDAJCAAAAAAAAAEBAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAJ')).!

plistWrongVersion
	"
		self createPlistMethod: 'plistWrongVersion' fromFile: 'plist_v2.bplist'
	"
	^##((ByteArray fromBase64String: 'YnBsaXN0MDEJCA0KDQoNCg0KDQoNCgEBDQoNCg0KDQoNCg0KDQoBDQoNCg0KDQoNCg0KDQoNCg0K
DQoNCg0KDQoNCg0KCQ0K')).!

propertyListTypeConstant
	^NSPropertyListBinaryFormat_v1_0! !
!BinaryPropertyListTest class categoriesFor: #filesAndMethods!public! !
!BinaryPropertyListTest class categoriesFor: #plistArray!public! !
!BinaryPropertyListTest class categoriesFor: #plistData!public! !
!BinaryPropertyListTest class categoriesFor: #plistDate!public! !
!BinaryPropertyListTest class categoriesFor: #plistDictionary!public! !
!BinaryPropertyListTest class categoriesFor: #plistFalse!public! !
!BinaryPropertyListTest class categoriesFor: #plistInteger!public! !
!BinaryPropertyListTest class categoriesFor: #plistNestedArray!public! !
!BinaryPropertyListTest class categoriesFor: #plistNestedDictionary!public! !
!BinaryPropertyListTest class categoriesFor: #plistNotAPlist!public! !
!BinaryPropertyListTest class categoriesFor: #plistReal!public! !
!BinaryPropertyListTest class categoriesFor: #plistString!public! !
!BinaryPropertyListTest class categoriesFor: #plistStringWithDoubleQuotes!public! !
!BinaryPropertyListTest class categoriesFor: #plistStringWithSingleQuotes!public! !
!BinaryPropertyListTest class categoriesFor: #plistTrue!public! !
!BinaryPropertyListTest class categoriesFor: #plistWrongVersion!public! !
!BinaryPropertyListTest class categoriesFor: #propertyListTypeConstant!public! !

