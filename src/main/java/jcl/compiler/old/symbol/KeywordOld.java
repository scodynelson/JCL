package jcl.compiler.old.symbol;

import jcl.structs.symbols.KeywordSymbolStruct;

public class KeywordOld {

	/**
	 * An instance of the Keyword Default
	 */
	public static final KeywordSymbolStruct Default = new KeywordSymbolStruct("DEFAULT");
	/**
	 * An instance of the Keyword Upcase
	 */
	public static final KeywordSymbolStruct Upcase = new KeywordSymbolStruct("UPCASE");
	/**
	 * An instance of the Keyword Internal
	 */
	public static final KeywordSymbolStruct Internal = new KeywordSymbolStruct("INTERNAL");
	/**
	 * An instance of the Keyword External
	 */
	public static final KeywordSymbolStruct External = new KeywordSymbolStruct("EXTERNAL");
	/**
	 * An instance of the Keyword Inherited
	 */
	public static final KeywordSymbolStruct Inherited = new KeywordSymbolStruct("INHERITED");
	/**
	 * An instance of the Keyword DownCase
	 */
	public static final KeywordSymbolStruct Downcase = new KeywordSymbolStruct("DOWNCASE");
	/**
	 * An instance of the Keyword Capitalize
	 */
	public static final KeywordSymbolStruct Capitalize = new KeywordSymbolStruct("CAPITALIZE");
	/**
	 * An instance of the Keyword Preserve
	 */
	public static final KeywordSymbolStruct Preserve = new KeywordSymbolStruct("PRESERVE");
	/**
	 * An instance of the Keyword Invert
	 */
	public static final KeywordSymbolStruct Invert = new KeywordSymbolStruct("INVERT");

	/* Keywords for compiler environment */
	public static final KeywordSymbolStruct LoadTimeValue = new KeywordSymbolStruct("LOAD-TIME-VALUE");
	/**
	 * An instance of the Keyword FIELD_NAME
	 */
	public static final KeywordSymbolStruct FieldName = new KeywordSymbolStruct("FIELD_NAME");
	/**
	 * An instance of the Keyword Binding
	 */
	public static final KeywordSymbolStruct Binding = new KeywordSymbolStruct("BINDING");
	/**
	 * An instance of the Keyword Bindings
	 */
	public static final KeywordSymbolStruct Bindings = new KeywordSymbolStruct("BINDINGS");
	/**
	 * An instance of the Keyword FBinding
	 */
	public static final KeywordSymbolStruct FletBinding = new KeywordSymbolStruct("FLET-BINDING");
	/**
	 * An instance of the Keyword FBindings
	 */
	public static final KeywordSymbolStruct FletBindings = new KeywordSymbolStruct("FLET-BINDINGS");
	/**
	 * An instance of the Keyword FBinding
	 */
	public static final KeywordSymbolStruct LabelsBinding = new KeywordSymbolStruct("LABELS-BINDING");
	/**
	 * An instance of the Keyword FBindings
	 */
	public static final KeywordSymbolStruct LabelsBindings = new KeywordSymbolStruct("LABELS-BINDINGS");
	/**
	 * An instance of the Keyword SymbolTable
	 */
	public static final KeywordSymbolStruct SymbolTable = new KeywordSymbolStruct("SYMBOL-TABLE");
	/**
	 * An instance of the Keyword Closure
	 */
	public static final KeywordSymbolStruct Closure = new KeywordSymbolStruct("CLOSURE");
	/**
	 * An instance of the Keyword Parent
	 */
	public static final KeywordSymbolStruct Parent = new KeywordSymbolStruct("PARENT");
	/**
	 * An instance of the Keyword Type
	 */
	public static final KeywordSymbolStruct Type = new KeywordSymbolStruct("TYPE");
	/**
	 * An instance of the Keyword Allocation
	 */
	public static final KeywordSymbolStruct Allocation = new KeywordSymbolStruct("ALLOCATION");
	/**
	 * An instance of the Keyword Scope
	 */
	public static final KeywordSymbolStruct Scope = new KeywordSymbolStruct("SCOPE");
	/**
	 * An instance of the Keyword InitForm
	 */
	public static final KeywordSymbolStruct InitForm = new KeywordSymbolStruct("INIT-FORM");
	/**
	 * An instance of the Keyword UnboundValue
	 */
	public static final KeywordSymbolStruct UnboundValue = new KeywordSymbolStruct("*UNBOUND-VALUE*");
	/**
	 * An instance of the Keyword Lexical
	 */
	public static final KeywordSymbolStruct Lexical = new KeywordSymbolStruct("LEXICAL");
	/**
	 * An instance of the Keyword Dynamic
	 */
	public static final KeywordSymbolStruct Dynamic = new KeywordSymbolStruct("DYNAMIC");
	/**
	 * An instance of the Keyword Parameter
	 */
	public static final KeywordSymbolStruct Parameter = new KeywordSymbolStruct("PARAMETER");
	/**
	 * An instance of the Keyword Local
	 */
	public static final KeywordSymbolStruct Local = new KeywordSymbolStruct("LOCAL");
	/**
	 * An instance of the Keyword References
	 */
	public static final KeywordSymbolStruct References = new KeywordSymbolStruct("REFERENCES");
	/**
	 * An instance of the Keyword Required
	 */
	public static final KeywordSymbolStruct Required = new KeywordSymbolStruct("REQUIRED");
	/**
	 * An instance of the Keyword Free
	 */
	public static final KeywordSymbolStruct Free = new KeywordSymbolStruct("FREE");
	/**
	 * An instance of the Keyword Position
	 */
	public static final KeywordSymbolStruct Position = new KeywordSymbolStruct("POSITION");
	/**
	 * An instance of the Keyword Depth
	 */
	public static final KeywordSymbolStruct Depth = new KeywordSymbolStruct("DEPTH");

	public static final KeywordSymbolStruct CompileToplevel = new KeywordSymbolStruct("COMPILE-TOPLEVEL");
	public static final KeywordSymbolStruct LoadToplevel = new KeywordSymbolStruct("LOAD-TOPLEVEL");
	public static final KeywordSymbolStruct Execute = new KeywordSymbolStruct("EXECUTE");

	/** Keywords for Pathnames
	 *  (NOTE:  An instance of the Keyword Type and Local are
	 *  already defined earlier in this file */
	/**
	 * An instance of the Keyword Absolute
	 */
	public static final KeywordSymbolStruct Absolute = new KeywordSymbolStruct("ABSOLUTE");
	/**
	 * An instance of the Keyword Relative
	 */
	public static final KeywordSymbolStruct Relative = new KeywordSymbolStruct("RELATIVE");
	/**
	 * An instance of the Keyword Wild
	 */
	public static final KeywordSymbolStruct Wild = new KeywordSymbolStruct("WILD");
	/**
	 * An instance of the Keyword Defaults
	 */
	public static final KeywordSymbolStruct Defaults = new KeywordSymbolStruct("DEFAULTS");
	/**
	 * An instance of the Keyword Unspecific
	 */
	public static final KeywordSymbolStruct Unspecific = new KeywordSymbolStruct("UNSPECIFIC");
	/**
	 * An instance of the Keyword Back
	 */
	public static final KeywordSymbolStruct Back = new KeywordSymbolStruct("BACK");
	/**
	 * An instance of the Keyword Case
	 */
	public static final KeywordSymbolStruct Case = new KeywordSymbolStruct("CASE");
	/**
	 * An instance of the Keyword Common
	 */
	public static final KeywordSymbolStruct Common = new KeywordSymbolStruct("COMMON");
	/**
	 * An instance of the Keyword Wild-inferior
	 */
	public static final KeywordSymbolStruct WildInferior = new KeywordSymbolStruct("WILD-INFERIOR");
	/**
	 * An instance of the Keyword Newest
	 */
	public static final KeywordSymbolStruct Newest = new KeywordSymbolStruct("NEWEST");
	/**
	 * An instance of the Keyword Oldest
	 */
	public static final KeywordSymbolStruct Oldest = new KeywordSymbolStruct("OLDEST");
	/**
	 * An instance of the Keyword Host.
	 */
	public static final KeywordSymbolStruct Host = new KeywordSymbolStruct("HOST");
	/**
	 * An instance of the Keyword Device.
	 */
	public static final KeywordSymbolStruct Device = new KeywordSymbolStruct("DEVICE");
	/**
	 * An instance of the Keyword Directory.
	 */
	public static final KeywordSymbolStruct Directory = new KeywordSymbolStruct("DIRECTORY");
	/**
	 * An instance of the Keyword Name.
	 */
	public static final KeywordSymbolStruct Name = new KeywordSymbolStruct("NAME");
	/**
	 * An instance of the Keyword Version.
	 */
	public static final KeywordSymbolStruct Version = new KeywordSymbolStruct("VERSION");
	/**
	 * An instance of the Keyword Start.
	 */
	public static final KeywordSymbolStruct Start = new KeywordSymbolStruct("START");
	/**
	 * An instance of the Keyword Start.
	 */
	public static final KeywordSymbolStruct End = new KeywordSymbolStruct("END");
	/**
	 * An instance of the Keyword Start.
	 */
	public static final KeywordSymbolStruct JunkAllowed = new KeywordSymbolStruct("JUNK-ALLOWED");

	/** Keywords added for URI support (these are not a part of Common Lisp) */
	/**
	 * An instance of the Keyword News.
	 */
	public static final KeywordSymbolStruct News = new KeywordSymbolStruct("news");
	/**
	 * An instance of the Keyword File. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordSymbolStruct File = new KeywordSymbolStruct("file");
	/**
	 * An instance of the Keyword HTTP. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordSymbolStruct HTTP = new KeywordSymbolStruct("http");
	/**
	 * An instance of the Keyword MAILTO. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordSymbolStruct MailTo = new KeywordSymbolStruct("mailto");
	/**
	 * An instance of the Keyword HTTPS. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordSymbolStruct HTTPS = new KeywordSymbolStruct("https");
	/**
	 * An instance of the Keyword PORT. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordSymbolStruct Port = new KeywordSymbolStruct("PORT");
	/**
	 * An instance of the Keyword USER-INFO. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordSymbolStruct UserInfo = new KeywordSymbolStruct("USER-INFO");

	/** Keywords added for Open.java **/
	/**
	 * An instance of the Keyword Direction.
	 */
	public static final KeywordSymbolStruct Direction = new KeywordSymbolStruct("DIRECTION");
	/**
	 * An instance of the Keyword Input.
	 */
	public static final KeywordSymbolStruct Input = new KeywordSymbolStruct("INPUT");
	/**
	 * An instance of the Keyword Output.
	 */
	public static final KeywordSymbolStruct Output = new KeywordSymbolStruct("OUTPUT");
	/**
	 * An instance of the Keyword IO.
	 */
	public static final KeywordSymbolStruct IO = new KeywordSymbolStruct("IO");
	/**
	 * An instance of the Keyword Probe.
	 */
	public static final KeywordSymbolStruct Probe = new KeywordSymbolStruct("PROBE");
	/**
	 * An instance of the Keyword ElementType.
	 */
	public static final KeywordSymbolStruct ElementType = new KeywordSymbolStruct("ELEMENT-TYPE");
	/**
	 * An instance of the Keyword Character.
	 */
	public static final KeywordSymbolStruct Character = new KeywordSymbolStruct("CHARACTER");
	/**
	 * An instance of the Keyword UnsignedByte.
	 */
	public static final KeywordSymbolStruct UnsignedByte = new KeywordSymbolStruct("UNSIGNED-BYTE");
	/**
	 * An instance of the Keyword SignedByte.
	 */
	public static final KeywordSymbolStruct SignedByte = new KeywordSymbolStruct("SIGNED-BYTE");
	/**
	 * An instance of the Keyword IfExists.
	 */
	public static final KeywordSymbolStruct IfExists = new KeywordSymbolStruct("IF-EXISTS");
	/**
	 * An instance of the Keyword Error.
	 */
	public static final KeywordSymbolStruct Error = new KeywordSymbolStruct("ERROR");
	/**
	 * An instance of the Keyword NewVersion.
	 */
	public static final KeywordSymbolStruct NewVersion = new KeywordSymbolStruct("NEW-VERSION");
	/**
	 * An instance of the Keyword Rename.
	 */
	public static final KeywordSymbolStruct Rename = new KeywordSymbolStruct("RENAME");
	/**
	 * An instance of the Keyword RenameAndDelete.
	 */
	public static final KeywordSymbolStruct RenameAndDelete = new KeywordSymbolStruct("RENAME-AND-DELETE");
	/**
	 * An instance of the Keyword Overwrite.
	 */
	public static final KeywordSymbolStruct Overwrite = new KeywordSymbolStruct("OVERWRITE");
	/**
	 * An instance of the Keyword Append.
	 */
	public static final KeywordSymbolStruct Append = new KeywordSymbolStruct("APPEND");
	/**
	 * An instance of the Keyword Supersede.
	 */
	public static final KeywordSymbolStruct Supersede = new KeywordSymbolStruct("SUPERSEDE");
	/**
	 * An instance of the Keyword IfDoesNotExist.
	 */
	public static final KeywordSymbolStruct IfDoesNotExist = new KeywordSymbolStruct("IF-DOES-NOT-EXIST");
	/**
	 * An instance of the Keyword Create.
	 */
	public static final KeywordSymbolStruct Create = new KeywordSymbolStruct("CREATE");
	/**
	 * An instance of the Keyword ExternalFormat.
	 */
	public static final KeywordSymbolStruct ExternalFormat = new KeywordSymbolStruct("EXTERNAL-FORMAT");

}
