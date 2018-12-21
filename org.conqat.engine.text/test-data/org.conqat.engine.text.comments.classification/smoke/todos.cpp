
// this is meant to show where TODOs can be placed in the code

// TODO: At the beginning $task§

template <class T>
// FIXME: in between $task§
class Foo {
public:
    // HACK: in methods $task§
	virtual ~Foo<T>() {}
	
    // TODO: and again $task§
	typename Foo<T>::iterator my_func(typename Foo<T>::iterator iter) {
		typename NamedObjectMap<T>::iterator incr = iter++;
	    // TODO: inline, of course $task§
		if (iter == Foo<T>::end()) {
			return Foo<T>::end();
		}
	    // TODO: inline, too $task§
		return Foo<T>::find(incr);
	}
};

// TODO(BH): Outside

template <class T>
void Foo<T>::initialize() {
	// TODO(BH): Inside
	log("Foo::initialize()"); 
}

// TODO try to get rid of this task
template <class T>
T& Foo<T>::operator*() {
	return 17;
}

