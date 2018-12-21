// ****************************************************************
// This is free software licensed under the NUnit license. You
// may obtain a copy of the license as well as information regarding
// copyright ownership at http://nunit.org.
// ****************************************************************

using System;
using System.Collections;
using System.Xml;
using System.Xml.Schema;
using System.IO;
using System.Threading;
using NUnit.Core;

namespace NUnit.Util
{
	/// <summary>
	/// Class that represents an NUnit test project
	/// </summary>
	public class NUnitProject
    {
        /// <summary>
		/// Path to the file storing this project
		/// </summary>
		private string projectPath;

		/// <summary>
		/// Completely unrelated
		/// </summary>
		private string basePath;

		/// <summary>
		/// The path to which a project will be saved.
		/// </summary>
		public string ProjectPath
		{
			get { return projectPath; }
			set 
			{
				projectPath = Path.GetFullPath( value );
				isDirty = true;
			}
		}

		/// <summary>
		/// Unrelated again
		/// </summary>
		public string DefaultBasePath
		{
			get { return Path.GetDirectoryName( projectPath ); }
		}
		
		/// For an indexer, the comment should be related in any case
		public string this[int index] {
			get { return Items[index]; }
			set { Items[index] = value;}
		}
		
		/// The same holds for operator overloading
		public static bool operator ==( TestName name1, TestName name2 )
		{
			return Object.Equals( name2, null );
		}
	}
}
