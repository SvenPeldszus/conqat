#include <iostream>

using std::cout;

int main () {
	try {
		cout << "do";
	} catch (...) {
		cout << "foo";
	}
}
