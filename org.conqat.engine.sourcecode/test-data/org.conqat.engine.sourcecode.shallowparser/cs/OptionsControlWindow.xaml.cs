using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace Cqse.Teamscale.Settings
{
	/// <summary>
	/// Implementation of a options control window that is ment for editing an item.
	/// The window consists of a save and cancel button as well as a control for hosting the actual editor.
	/// Item modification will be saved after clicking the save button and discarded clicking cancel or closing the dialog.
	/// </summary>
	public partial class OptionsControlWindow : Window
	{
		/// <summary>
		/// The user control for editing the item.
		/// </summary>
		private IEditorControl editorControl;

		/// <summary>
		/// Backup of the edited item properties.
		/// </summary>
		private Dictionary<string, object> itemBackup = new Dictionary<string, object>();

		/// <summary>
		/// Creates a new instance of the <see cref="OptionsControlWindow"/> class.
		/// </summary>
		/// <param name="hostedContent">The editor control that is hosted.</param>
		public OptionsControlWindow(IEditorControl editorControl)
		{
			this.editorControl = editorControl;

			this.CreateItemBackup();
			InitializeComponent();

			this.HostedConrol.Content = editorControl.AsUserControl();
		}

		private async void OnSaveClicked(object sender, RoutedEventArgs e)
		{
			bool result = await Validate();
			if (result)
			{
				DialogResult = true;
				this.Close();
			}
		}

		private async Task<bool> Validate()
		{
			var task = new Task<bool>(() => { return true; });

			task.Start();

			return await task;
		}

		/// <inheritdoc/>
		protected override void OnClosed(EventArgs e)
		{
			// Restore backup if the dialog is canceled.
			if (DialogResult != true)
			{
				this.RestoreItemBackup();
			}
		}


		/// <summary>
		/// Backs up all accessible properties of the edited item, so modification can be undone upon canceling the dialog.
		/// </summary>
		private void CreateItemBackup()
		{
			var item = this.editorControl.Item;
			foreach (var property in GetAccessibleProperties(item))
			{
				this.itemBackup[property.Name] = property.GetValue(item, null);
			}
		}

		/// <summary>
		/// Restores the item property values from the backup.
		/// </summary>
		private void RestoreItemBackup()
		{
			var item = this.editorControl.Item;
			foreach (var property in GetAccessibleProperties(item))
			{
				property.SetValue(item, this.itemBackup[property.Name], null);
			}
		}

		/// <summary>
		/// Returns all accessible (i.e. readable and writable) properties of an object.
		/// </summary>
		/// <param name="o">The object the accessible properties should be read from.</param>
		/// <returns>The accessible properties.</returns>
		private static IEnumerable<PropertyInfo> GetAccessibleProperties(object o)
		{
			return o.GetType().GetProperties().Where(property => property.CanRead && property.CanWrite);
		}

		/// <summary>
		/// Interface that have to be implemented by controls hosted by <see cref="ServerOptionsControl"/>.
		/// </summary>
		public interface IEditorControl
		{
			/// <summary>
			/// The item that is edited by the control.
			/// </summary>
			object Item { get; }

			/// <summary>
			/// 
			/// </summary>
			/// <returns>The user control that is designed for editing the item. Usually just returns <code>this</code>.</returns>
			UserControl AsUserControl();
		}

		/// <summary>
		/// Generic variant of <see cref="IEditorControl"/>.
		/// </summary>
		/// <typeparam name="T">The type of the item that is edited by the control.</typeparam>
		public interface IEditorControl<T> : IEditorControl
		{
			/// <summary>
			/// The item that is edited by the control.
			/// </summary>
			new T Item { get; }
		}
	}
}
