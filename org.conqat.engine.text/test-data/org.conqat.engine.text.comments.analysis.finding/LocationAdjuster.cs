// <copyright file="$Id: NormalizedDictionary.cs 13713 2014-07-18 06:05:03Z poehlmann $" company="CQSE GmbH">
//     Copyright (c)  2009-2014 CQSE GmbH
// </copyright>
// <author>$Author: poehlmann $</author>
// <version>$Rev: 13713 $</version>
// <rating>@ConQAT.Rating GREEN Hash: 7A91A10A25293ACF045D65A46D7100B1</rating>

namespace Cqse.Teamscale.VisualStudio.Util
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Text;
	using System.Text.RegularExpressions;
	using Cqse.Common.Util.Algo;
	using Cqse.Common.Util.Region;
	using Cqse.Common.Util.String;
	using Cqse.Teamscale.VisualStudio.Model;

	/// <summary>
	/// This class is used for adjusting the offsets used in locations (i.e.
	/// subclasses of {@link ElementLocation} for text that is slightly modified. The
	/// main use-case is the update of locations where the local (adjusted) text has
	/// different line ending, different content due to keyword expansion, or minor
	/// local modifications compared to the text on which the analysis was executed
	/// (original text).
	///
	/// Both the original and adjusted text may have arbitrary line endings.
	///
	/// The implementation is based on a token diff, which can lead to minor
	/// deviations for offsets that are not aligned with token boundaries. A
	/// character diff would be more precise, but is too performance and memory
	/// intensive for large files.
	/// </summary>
	public class LocationAdjuster
	{


		/// <summary>
		/// Simple token representation used in location adjustment.
		/// </summary>
		private class AdjusterToken
		{
			/// <summary>
			/// The text content.
			/// </summary>
			private string text;

			/// <summary>
			/// The start offset in the text.
			/// </summary>
			public int StartOffset { get; private set; }

			/// <summary>
			/// The inclusive end offset in the text.
			/// </summary>
			public int EndOffset { get; private set; }

			/// <summary>
			/// Constructor.
			/// </summary>
			public AdjusterToken(String text, int startOffset)
				: this(text, startOffset, startOffset + text.Length - 1)
			{
			}

			/// <summary>
			/// Constructor.
			/// </summary>
			public AdjusterToken(String text, int startOffset, int endOffset)
			{
				this.text = text;
				this.StartOffset = startOffset;
				this.EndOffset = endOffset;
			}

			/// <inheritdoc/>
			public override bool Equals(Object obj)
			{
				AdjusterToken token = obj as AdjusterToken;
				if (token == null)
				{
					return false;
				}
				return token.text.Equals(text);
			}

			/// <inheritdoc/>
			public override int GetHashCode()
			{
				return text.GetHashCode();
			}
		}
	}
}