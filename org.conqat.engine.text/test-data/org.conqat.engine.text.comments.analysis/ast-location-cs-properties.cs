//HEADER
namespace Cqse.Teamscale.Editor
{
	//HEADER
	public abstract class TextViewMarginBase : Canvas, IWpfTextViewMargin
	{
		//INTERFACE
		public double MarginSize
		{
			//INLINE
			get
			{   //INLINE
				this.ThrowIfDisposed();
				return this.ActualHeight;
				//INLINE
			}
			//INLINE
			
			//INLINE
			set {
				//INLINE
				this.foo = value;
				//INLINE
			}
			//INLINE
		}
		//INTERFACE
	}
	//HEADER
}
//HEADER