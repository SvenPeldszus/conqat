class WithFriends {
 public:
    WithFriends ();
    ~WithFriends ();
    
    void someMethod ();
    
    friend Q_EXPORT Stream& operator<<(Stream &, WithFriends &);

 private:
 	int state;
 	double startTime;
}
