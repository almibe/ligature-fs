# Ligature Lab

This project provides a backend sever for working with Ligature.

## API

Currently this project supports a single end point.
This endpoint will accept a Wander script and execute it and return the value requested.

POST /wander Body = "true" -> Result 200 "true"