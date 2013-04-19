% LS1MCS and Ground Segment Architecture
% Karolis Petrauskas <k.petrauskas@gmail.com>
% 2013-04-20

Introduction
============

Purpose of the document
-----------------------

Document structure
------------------

Editing the document
--------------------

Revision history
----------------

--------------------------------------------------------------------------
Date         Author              Comments
------------ ------------------- -----------------------------------------
2013-04-11   K. Petrauskas       Initial revision
2013-04-20   K. Petrauskas       Deployment view detailed.
--------------------------------------------------------------------------
Table: Revision history.


Scope definition
================

Requirements overview
=====================

Architecture decisions
======================

This section outlines main architecture decisions.
The ground segment, its interfaces and context is shown in the diagram bellow.

![Context of the LS1 ground segment](uml/Component_Diagram__GroundSegment__Ground_segment_-_context.png)

The ground station uses `LS1P` interface (and protocol) to communicate with the "Lituanica SAT-1" and
provide the following interfaces:

Mission control GUI (MCS)
:   A web based user interface allowing to communicate with the SAT. This interface is private and secured.
Telemetry publishing GUI (PUB)
:   A web based user interface for reviewing telemetry archive,
    HAM submitted telemetry and a current state of the SAT.
    This interface is planned to be publicly accessible and provide read access only.
Telemetry submission GUI (HAM)
:   A web based user interface for HAMs to submit telemetry packets as well as to decode them.
    This interface will be publicly available and will have write access to the telemetry database.
Telemetry API
:   Its a REST api providing programming interface to the telemetry database.
    This interface is planned to be public and will provide read access only.


Main architectural decisions and arguments for them:

Erlang as a platform for the backend.
:   ...

Web server
:   ...

Database
:   ...

Single page WEB applications.
:   The web based user interfaces are built as single page applications.
    All the user interface is build using JavaScript.
    The user interface access and modifies related data via REST services.
    Three main reasons for this decision:

      * Majority of today's applications provide web interface.
      * JavaScript applications are more interactive than applications based on server side HTML rendering.
      * Its easier to separate user interface (the JavaScript and the HTML part)
        from the business logic (provided by the server side, via Ajax).



Information architecture view
=============================

Functional view
===============


![Ground segment decomposition](uml/Composite_Structure_Diagram__Ground_segment__Ground_station_-_structure.png)


![MCS components](uml/Component_Diagram__LS1MCS__LS1MCS_-_components.png)



Process view
============

Deployment view
===============


![Dislocation of components composing LS1 ground segment](uml/Deployment_Diagram__Deloyment__Ground_segment_-_deployment.png)


Non functional aspects
======================

Summary
=======


