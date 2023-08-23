# Run these commands in the command prompt to start up
pip install property
potrey install
poetry shell

# To add/remove a package into the project, use the following commands
poetry add pandas
poetry remove numpy

# Run Precommit before committing the github changes, as it will reformat the files before sending
inv precommit