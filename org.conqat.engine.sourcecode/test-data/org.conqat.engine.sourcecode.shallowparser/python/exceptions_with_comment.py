try:
	pass
except ImportError:
	# tkinter may be missing
	pass
return

try:
	pass
except locale.Error: # Workaround for "locale.Error: unsupported locale setting"
	pass
return