//
// decl.cs: Declaration base class for structs, classes, enums and interfaces.
//
// Author: Miguel de Icaza (miguel@gnu.org)
//         Marek Safar (marek.safar@seznam.cz)
//
// Dual licensed under the terms of the MIT X11 or GNU GPL
//
// Copyright 2001 Ximian, Inc (http://www.ximian.com)
// Copyright 2004-2008 Novell, Inc
// Copyright 2011 Xamarin Inc
//
//

namespace Mono.CSharp {

	/// <summary>
	///   Base representation for members.  This is used to keep track
	///   of Name, Location and Modifier flags, and handling Attributes.
	/// </summary>
	[System.Diagnostics.DebuggerDisplay ("{GetSignatureForError()}")]
	public abstract class MemberCore : Attributable, IMemberContext, IMemberDefinition
	{

		//
		// Checks whether the type P is as accessible as this member
		//
		public bool IsAccessibleAs (TypeSpec p)
		{

			for (TypeSpec p_parent; p != null; p = p_parent) {
				for (MemberCore mc = this; !same_access_restrictions && mc != null && mc.Parent != null; mc = mc.Parent) {
					var al = mc.ModFlags & Modifiers.AccessibilityMask;
					switch (pAccess) {
					case Modifiers.INTERNAL:
						if (al == Modifiers.PRIVATE || al == Modifiers.INTERNAL)
							same_access_restrictions = p.MemberDefinition.IsInternalAsPublic (mc.Module.DeclaringAssembly);
						
						break;

					case Modifiers.PROTECTED:
						if (al == Modifiers.PROTECTED) {
							same_access_restrictions = mc.Parent.PartialContainer.IsBaseTypeDefinition (p_parent);
							break;
						}

						if (al == Modifiers.PRIVATE) {
							//
							// When type is private and any of its parents derives from
							// protected type then the type is accessible
							//
							while (mc.Parent != null && mc.Parent.PartialContainer != null) {
								if (mc.Parent.PartialContainer.IsBaseTypeDefinition (p_parent))
									same_access_restrictions = true;
								mc = mc.Parent; 
							}
						}
						
						break;

					case Modifiers.PROTECTED | Modifiers.INTERNAL:
						if (al == Modifiers.INTERNAL)
							same_access_restrictions = p.MemberDefinition.IsInternalAsPublic (mc.Module.DeclaringAssembly);
						else if (al == (Modifiers.PROTECTED | Modifiers.INTERNAL))
							same_access_restrictions = mc.Parent.PartialContainer.IsBaseTypeDefinition (p_parent) && p.MemberDefinition.IsInternalAsPublic (mc.Module.DeclaringAssembly);
						else
							goto case Modifiers.PROTECTED;

						break;

					case Modifiers.PRIVATE:
						//
						// Both are private and share same parent
						//
						if (al == Modifiers.PRIVATE) {
							var decl = mc.Parent;
							do {
								same_access_restrictions = decl.CurrentType.MemberDefinition == p_parent.MemberDefinition;
							} while (!same_access_restrictions && !decl.PartialContainer.IsTopLevel && (decl = decl.Parent) != null);
						}
						
						break;
						
					default:
						throw new InternalErrorException (al.ToString ());
					}
				}
				
				if (!same_access_restrictions)
					return false;
			}

			return true;
		}
	}
}
