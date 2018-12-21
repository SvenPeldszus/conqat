// <copyright file="$Id$" company="CQSE GmbH">
//     Copyright (c)  2009-2013 CQSE GmbH   
// </copyright>
// <author>$Author$</author>
// <version>$Rev$</version>
// <rating>@ConQAT.Rating GREEN Hash: 2D34C8630544AC779F97FC47E4CF7382</rating>

namespace Cqse.Teamscale.Editor
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Text;
	using System.Threading.Tasks;
	using System.Windows.Controls;
	using Microsoft.VisualStudio.Text.Editor;

	/// <summary>  §header§
	/// Base implementation for text view margins painting on a canvas.
	/// </summary>
	public abstract class TextViewMarginBase : Canvas, IWpfTextViewMargin
	{
		/// <summary>  §interface§
		/// Flag for determining if the margin is disposed.
		/// </summary>
		private bool isDisposed = false;

		/// <summary>  §interface§
		/// Gets the <see cref="Sytem.Windows.FrameworkElement"/> that implements the visual representation
		/// of the margin.
		/// </summary>
		public System.Windows.FrameworkElement VisualElement
		{
			// Since this margin implements Canvas, this is the object which renders  §inline§
			// the margin.
			get
			{
				this.ThrowIfDisposed();
				return this;
			}
		}

		/// <summary>  §interface§
		/// Gets the height of the margin.
		/// </summary>
		public double MarginSize
		{
			// Since this is a horizontal margin, its width will be bound to the width of the text view.  §inline§
			// Therefore, its size is its height.
			get
			{
				this.ThrowIfDisposed();
				return this.ActualHeight;
			}
		}

		/// <summary>  §interface§
		/// Gets a value indicating whether the margin is enabled.
		/// </summary>
		public bool Enabled
		{
			get
			{
				this.ThrowIfDisposed();

				// For now margin should always be visible until we have an configuration option to disable it.  §inline§
				return true;
			}
		}

		/// <summary>  §interface§
		/// Returns this margin instance if this is the margin that has been requested.
		/// </summary>
		/// <param name="marginName">The name of the margin requested.</param>
		/// <returns>This instance of EditorMargin or null.</returns>
		public ITextViewMargin GetTextViewMargin(string marginName)
		{
			return (marginName == FindingMargin.MarginIdentifier) ? (IWpfTextViewMargin)this : null;
		}

		/// <summary>  §interface§
		/// Disposes the widget.
		/// </summary>
		public void Dispose()
		{
			if (!this.isDisposed)
			{
				GC.SuppressFinalize(this);
				this.OnDispose();
				this.isDisposed = true;
			}
		}

		/// <summary>  §interface§
		/// Called upon disposing this widget. Meant to be overridden by subclasses.
		/// </summary>
		protected virtual void OnDispose()
		{
			// stub to be implemented by sub classes.
		}

		/// <summary>  §interface§
		/// Returns an unique identifier for each margin class.
		/// </summary>
		/// <returns>The unique identifier for each margin class.</returns>
		protected abstract string GetMarginIdentifier();

		/// <summary>  §interface§
		/// Throws an exception if the margin is disposed.
		/// </summary>
		private void ThrowIfDisposed()
		{
			if (this.isDisposed)
			{
				throw new ObjectDisposedException(this.GetMarginIdentifier());
			}
		}
	}
}
