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

-------------------------------------------------------------------------------------
Date         Author              Comments
------------ ------------------- ----------------------------------------------------
2013-04-11   K. Petrauskas       Initial revision

2013-04-20   K. Petrauskas       Deployment view detailed.

-------------------------------------------------------------------------------------
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

Mission control GUI (MCS).
:   A web based user interface allowing to communicate with the SAT. This interface is private and secured.
Telemetry publishing GUI (PUB).
:   A web based user interface for reviewing telemetry archive,
    HAM submitted telemetry and a current state of the SAT.
    This interface is planned to be publicly accessible and provide read access only.
Telemetry submission GUI (HAM).
:   A web based user interface for HAMs to submit telemetry packets as well as to decode them.
    This interface will be publicly available and will have write access to the telemetry database.
Telemetry API.
:   Its a REST api providing programming interface to the telemetry database.
    This interface is planned to be public and will provide read access only.


Main architectural decisions and arguments for them:

Erlang as a platform for the backend.
:   At the beginning fo the project the [Hummingbird](http://www.hbird.de/) was considered as
    a platform for the Lituanica SAT-1 ground segment implementation. The main concern with
    this software was its stability and predictiveness at operations. The concerns were mostly
    because Apache Camel was used in this solution along with number of ActiveMQ queues.

    The main reason for selecting Erlang based solution instead of one described above is
    stability of the platform. Apart of that, protocols are easy to implement in erlang because
    of great support for finite state machines and binary pattern matching.

Web server.
:   Yaws was selected as a web server for implementing REST api as well as for serving WEB GUIs.
    This product was selected because it is Erlang based server (fits well into the stack) and
    the Erlang team of this project had experience with this web server.

Database.
:   Basho Riak was selected as a database. Its erlang based solution and has good support for binary data.
    Considered alternatives were: Mnesia and PostgreSQL.

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

![Information model.](uml/Class_Diagram__DataModel__Data_model.png)


Functional view
===============


![Ground segment decomposition.](uml/Composite_Structure_Diagram__Ground_segment__Ground_station_-_structure.png)


![MCS components.](uml/Component_Diagram__LS1MCS__LS1MCS_-_components.png)



Process view
============

Deployment view
===============

The LS1MCS is designed to work on Linux. For security reasons the MCS is deployed on several servers.
The following diagram shows devices and servers composing the run-time environment for the MCS.

![Dislocation of components composing LS1 ground segment](uml/Deployment_Diagram__Deloyment__Ground_segment_-_deployment.png)

The following diagram shows particular instances composing the MCS as well as communication links between them.

![Node and artifact instances](uml/Deployment_Diagram__Deloyment__Ground_segment_-_instances.png)

The `liepiskes` server will be running in the main mission control centre.
TNC, transceiver and the anthena rotator will be connected to its physically via COM ports.
This server will only run RIAK database, GPredics LS1MCS Core module and the SAT commanding GUI.
The PUB and HAM GUIs are deployed on separate server for security reasons.
For the same reasons, the backup server will be separated from the WEB GUI server.

The `mif-backup` server will have no access to the core server. The data to backup will be pushed to it.
The `mif-web` server will be accessing the `liepiskes` server via REST protocol. The security will be
managed here by using a source host filtering on the `liepiskes` firewall as well as the `web_api` component.


Non functional aspects
======================

Summary
=======


