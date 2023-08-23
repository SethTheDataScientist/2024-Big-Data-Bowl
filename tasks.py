from invoke import task


@task
def precommit(c):
    """
    Runs the files in the Code folder through reformatting
    """
    c.run(f"isort code")
    c.run(f"black code")
    c.run(f"flake8 code")