// <copyright file="$Id$" company="CQSE GmbH">
// Copyright (c) 2009-2014 CQSE GmbH
// </copyright>
// <author>$Author$</author>
// <version>$Rev$</version>
// <rating>@ConQAT.Rating YELLOW Hash: 9D0C7D482735B0AA494759F12BCAA4E7</rating>

namespace Cqse.Teamscale.Settings
{
	using System.Collections.Generic;
	using System.Collections.ObjectModel;
	using System.Linq;
	using System.Windows;
	using System.Windows.Controls;

	/// <summary> §header§
	/// Generic base implementation of a <see cref="EditableListControl"/>.
	/// </summary>
	/// <typeparam name="T">The types of objects shown in the list view.</typeparam>
	public abstract class EditableListControl<T> : EditableListControl
	{
		/// <summary>  §interface§
		/// Initializes a new instance of the <see cref="EditableListControl{T}"/> class.
		/// </summary>
		/// <param name="items">The list of items shown in the list view.</param>
		public EditableListControl(ObservableCollection<T> items)
			: base(items)
		{
			// just proxy
		}

		/// <inheritdoc/>  §interface§
		protected override OptionsControlWindow.IEditorControl CreateEditorControl(object item)
		{
			return this.CreateEditorControl((T)item);
		}

		/// <summary> §interface§
		/// Creates a user control for editing the specified item.
		/// </summary>
		/// <param name="item">The item to be edited.</param>
		/// <returns>The editor control.</returns>
		protected abstract OptionsControlWindow.IEditorControl<T> CreateEditorControl(T item);
	}
}
		
