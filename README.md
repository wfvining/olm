# OCPP

An OTP application for communication with electric vehicle charging
stations using the Open Charge Point Protocol. 

Version 1.0 of the application is a complete rewrite of the version
0.1 prototype. Lessons learned from the first version have been taken
where appropriate and many improvements are added. The most notable
changes are listed below.

## Changes from Version 0.1

### Enable Dialyzer to Check Message Construction

A major frustration with the prototype version was that message
construction was validated strictly at runtime. This lead to
inscrutable error messages from the `jerk** application and irritating
debugging loops while trying to figure out what parameter you passed
incorrectly. In version 1.0 we provide messages construction functions
for each message type along with type definitions for the messages
themselves. Using these constructors it is possible for Dialyzer to
check the maps provided for the correct types and required keys prior
to run time. Of course, run time checking is still required for value
constraints; however, error messages from bad values have been
improved.

### Support Multiple OCPP Versions

*TODO*
