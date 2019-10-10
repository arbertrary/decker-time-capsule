# SCORM Package

1. (API) Run-Time Environment (how content behaves in LMS) - js files
2. Content Aggregation Model (how content is packaged) - xml files

- zip file
- imsmanifest.xml at root 
- all resource files
- metadata.xml
- sequencing xml binding

# API Calls

Content should call GetAPI() to locate LMS SCORM API adapter. This should be called from the top parent window of the content. 

# Required API Methods

[Cheat](https://scorm.com/scorm-explained/technical-scorm/run-time/run-time-reference/)

API.LMSInitialize( “” ) : bool 
– Begins a communication session with the LMS.
API.LMSFinish( “” ) : bool 
– Ends a communication session with the LMS.
API.LMSGetValue( element : CMIElement ) : string 
– Retrieves a value from the LMS.
API.LMSSetValue( element : CMIElement, value : string) : string 
– Saves a value to the LMS.
API.LMSCommit( “” ) : bool 
– Indicates to the LMS that all data should be persisted (not required).
API.LMSGetLastError() : CMIErrorCode 
– Returns the error code that resulted from the last API call.
API.LMSGetErrorString( errorCode : CMIErrorCode ) : string 
– Returns a short string describing the specified error code.
API.LMSGetDiagnostic( errorCode : CMIErrorCode ) : string 
– Returns detailed information about the last error that occurred.

# Content Aggregation Model

The Content Model describes how the content is delivered, relationships between modules, physical structure of files. The metadata escribes the content with a pre-defined vocabulary broken into 9 categories: 
General, Lifecycle, Meta-metadata, Technical, Educational, Rights, Relation, Annotation, Classification. All files must be zipped. At root must be imsmanifest.xml.

# Content Packaging

Manifest file lists all resources as SCOs or Assets. 
"Resources" are content objects that is a web-deliverable learning unit (html page).
An "asset" is an img, pdf, etc
A SCO is a launchable learning unit that can communicate directly with the LMS.

# Manifest file

Will need to list all resources in file tag with href. 

- Haskell builds beginning of imsmanifest.xml and appends resources. 
- item identifierref is the resource identifier for the launchable page
- title tag under item is the title of the learning module

Haskell functions: 
listDirectory :: FilePath -> IO[FilePath]
list objects in a directory excluding '.' and '..'. 

isDirectory :: FilePath -> IO Bool
if not continue, else listDirectory

put these in <file href="" />

zip file

# Resource tag

## Attributes

- *identifier* (required) an identifier unique within the manifest
- *type* (required) the resource type, shall be set to "webcontent"
- *adlcp:scormType* (required) the resource SCORM type, shall be set to "sco" or "asset"
- *href* (optional) URL of the "launching point" of the resource
- *xml:base* (optional) relative path offset for the files contained in the manifest/resource

## Children

<metadata> (optional) resource metadata
<file> (required) defines a file used by the resource - attributes *href*
<dependency> (optional) resource metadata

# Metadata

Describes the elements of a package
LOM: Learning Object Metadata

# Shareable Content Object

A launchable learning object communicates with run-time environment
Can be launched in stand-alone window or a frame

# Required SCO run-time behaviors:

- Find the RTE API instance provided by the LMS
- Use the API instance to initialize communication with the LMS
- Use the API instance to terminate communication with the LMS

# Recommended SCO behaviors:

- A SCO should be reusable in different learning contexts
- A SCO should be independent of visual constraints, such as window size
- A SCO should reliably transmit learner data so that it is not lost if closed unexpectedly
- A SCO should communicate its completion status
- A SCO should NOT launch new browser windows without closing them when done
- A SCO should NOT link to other files in the content package not listed as resource files of the SCO in the manifest

# Restricted SCO behaviors:

- A SCO may NOT interact with the run-time environment in any way other than the provided run-time API
- A SCO may NOT attempt to change the size or appearance of the run-time environment it is launched in
- A SCO may NOT close the top-level browser window it is launched in unless it is the only thing in the window

# Typical SCO Lifecycle

- SCO is launched by a SCORM Run-Time Environment (RTE) (often an LMS)
- SCO finds RTE provided API
- SCO begins communication with the RTE API (via a call to Initialize())
- Learner begins interaction with the SCO
- SCO sends and retreives data via the RTE API (via calls to Get/SetValue())
- Learner ends interaction with the SCO
- SCO ends communication with the RTE API (via a call to Terminate())



# Capture

Score
Total time spent in module
Time spent in a single session
Completion Status
Responses to Assessment items
Interactions within 
Pass Fail
Where learner left off
