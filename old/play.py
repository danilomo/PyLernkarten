regex = "(upload\.wikimedia\.org\/wikipedia\/commons\/[^\/]+\/[^\/]+\/[^\/]+\.ogg)"
>>> re.search(regex,x).group(1)
'upload.wikimedia.org/wikipedia/commons/a/a3/De-ankreuzen.ogg'
