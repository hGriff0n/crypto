# I have a lot of work getting this to work
all: crypto

crypto:
    scalac *.scala

# Get error, don't know how to clean
clean:
    rmdir crypto