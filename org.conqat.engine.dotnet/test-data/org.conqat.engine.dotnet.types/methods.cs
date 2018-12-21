namespace A {

    public abstract class Controller<CONTEXTOBJ> : Parent
        where CONTEXTOBJ : class, ILockable, new()
    {
        #region R1
        private void Foo() {
        }
        #endregion

        #region Properties
        public CONTEXTOBJ ContextObject(int a)
        {
                return _contextObject;
        }
        #endregion
    }//class
}//namespace
