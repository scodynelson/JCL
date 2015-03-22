package jcl.compiler.old.symbol;

import jcl.symbols.KeywordStruct;

public class KeywordOld {

	/**
	 * An instance of the Keyword Default
	 */
	public static final KeywordStruct Default = new KeywordStruct("DEFAULT");
	/**
	 * An instance of the Keyword Upcase
	 */
	public static final KeywordStruct Upcase = new KeywordStruct("UPCASE");
	/**
	 * An instance of the Keyword Internal
	 */
	public static final KeywordStruct Internal = new KeywordStruct("INTERNAL");
	/**
	 * An instance of the Keyword External
	 */
	public static final KeywordStruct External = new KeywordStruct("EXTERNAL");
	/**
	 * An instance of the Keyword Inherited
	 */
	public static final KeywordStruct Inherited = new KeywordStruct("INHERITED");
	/**
	 * An instance of the Keyword DownCase
	 */
	public static final KeywordStruct Downcase = new KeywordStruct("DOWNCASE");
	/**
	 * An instance of the Keyword Capitalize
	 */
	public static final KeywordStruct Capitalize = new KeywordStruct("CAPITALIZE");
	/**
	 * An instance of the Keyword Preserve
	 */
	public static final KeywordStruct Preserve = new KeywordStruct("PRESERVE");
	/**
	 * An instance of the Keyword Invert
	 */
	public static final KeywordStruct Invert = new KeywordStruct("INVERT");

	/* Keywords for compiler environment */
	public static final KeywordStruct LoadTimeValue = new KeywordStruct("LOAD-TIME-VALUE");
	/**
	 * An instance of the Keyword FIELD_NAME
	 */
	public static final KeywordStruct FieldName = new KeywordStruct("FIELD_NAME");
	/**
	 * An instance of the Keyword Binding
	 */
	public static final KeywordStruct Binding = new KeywordStruct("BINDING");
	/**
	 * An instance of the Keyword Bindings
	 */
	public static final KeywordStruct Bindings = new KeywordStruct("BINDINGS");
	/**
	 * An instance of the Keyword FBinding
	 */
	public static final KeywordStruct FletBinding = new KeywordStruct("FLET-BINDING");
	/**
	 * An instance of the Keyword FBindings
	 */
	public static final KeywordStruct FletBindings = new KeywordStruct("FLET-BINDINGS");
	/**
	 * An instance of the Keyword FBinding
	 */
	public static final KeywordStruct LabelsBinding = new KeywordStruct("LABELS-BINDING");
	/**
	 * An instance of the Keyword FBindings
	 */
	public static final KeywordStruct LabelsBindings = new KeywordStruct("LABELS-BINDINGS");
	/**
	 * An instance of the Keyword SymbolTable
	 */
	public static final KeywordStruct SymbolTable = new KeywordStruct("SYMBOL-TABLE");
	/**
	 * An instance of the Keyword Closure
	 */
	public static final KeywordStruct Closure = new KeywordStruct("CLOSURE");
	/**
	 * An instance of the Keyword Parent
	 */
	public static final KeywordStruct Parent = new KeywordStruct("PARENT");
	/**
	 * An instance of the Keyword Type
	 */
	public static final KeywordStruct Type = new KeywordStruct("TYPE");
	/**
	 * An instance of the Keyword Allocation
	 */
	public static final KeywordStruct Allocation = new KeywordStruct("ALLOCATION");
	/**
	 * An instance of the Keyword Scope
	 */
	public static final KeywordStruct Scope = new KeywordStruct("SCOPE");
	/**
	 * An instance of the Keyword InitForm
	 */
	public static final KeywordStruct InitForm = new KeywordStruct("INIT-FORM");
	/**
	 * An instance of the Keyword UnboundValue
	 */
	public static final KeywordStruct UnboundValue = new KeywordStruct("*UNBOUND-VALUE*");
	/**
	 * An instance of the Keyword Lexical
	 */
	public static final KeywordStruct Lexical = new KeywordStruct("LEXICAL");
	/**
	 * An instance of the Keyword Dynamic
	 */
	public static final KeywordStruct Dynamic = new KeywordStruct("DYNAMIC");
	/**
	 * An instance of the Keyword Parameter
	 */
	public static final KeywordStruct Parameter = new KeywordStruct("PARAMETER");
	/**
	 * An instance of the Keyword Local
	 */
	public static final KeywordStruct Local = new KeywordStruct("LOCAL");
	/**
	 * An instance of the Keyword References
	 */
	public static final KeywordStruct References = new KeywordStruct("REFERENCES");
	/**
	 * An instance of the Keyword Required
	 */
	public static final KeywordStruct Required = new KeywordStruct("REQUIRED");
	/**
	 * An instance of the Keyword Free
	 */
	public static final KeywordStruct Free = new KeywordStruct("FREE");
	/**
	 * An instance of the Keyword Position
	 */
	public static final KeywordStruct Position = new KeywordStruct("POSITION");
	/**
	 * An instance of the Keyword Depth
	 */
	public static final KeywordStruct Depth = new KeywordStruct("DEPTH");

	public static final KeywordStruct CompileToplevel = new KeywordStruct("COMPILE-TOPLEVEL");
	public static final KeywordStruct LoadToplevel = new KeywordStruct("LOAD-TOPLEVEL");
	public static final KeywordStruct Execute = new KeywordStruct("EXECUTE");

	/** Keywords for Pathnames
	 *  (NOTE:  An instance of the Keyword Type and Local are
	 *  already defined earlier in this file */
	/**
	 * An instance of the Keyword Absolute
	 */
	public static final KeywordStruct Absolute = new KeywordStruct("ABSOLUTE");
	/**
	 * An instance of the Keyword Relative
	 */
	public static final KeywordStruct Relative = new KeywordStruct("RELATIVE");
	/**
	 * An instance of the Keyword Wild
	 */
	public static final KeywordStruct Wild = new KeywordStruct("WILD");
	/**
	 * An instance of the Keyword Defaults
	 */
	public static final KeywordStruct Defaults = new KeywordStruct("DEFAULTS");
	/**
	 * An instance of the Keyword Unspecific
	 */
	public static final KeywordStruct Unspecific = new KeywordStruct("UNSPECIFIC");
	/**
	 * An instance of the Keyword Back
	 */
	public static final KeywordStruct Back = new KeywordStruct("BACK");
	/**
	 * An instance of the Keyword Case
	 */
	public static final KeywordStruct Case = new KeywordStruct("CASE");
	/**
	 * An instance of the Keyword Common
	 */
	public static final KeywordStruct Common = new KeywordStruct("COMMON");
	/**
	 * An instance of the Keyword Wild-inferior
	 */
	public static final KeywordStruct WildInferior = new KeywordStruct("WILD-INFERIOR");
	/**
	 * An instance of the Keyword Newest
	 */
	public static final KeywordStruct Newest = new KeywordStruct("NEWEST");
	/**
	 * An instance of the Keyword Oldest
	 */
	public static final KeywordStruct Oldest = new KeywordStruct("OLDEST");
	/**
	 * An instance of the Keyword Host.
	 */
	public static final KeywordStruct Host = new KeywordStruct("HOST");
	/**
	 * An instance of the Keyword Device.
	 */
	public static final KeywordStruct Device = new KeywordStruct("DEVICE");
	/**
	 * An instance of the Keyword Directory.
	 */
	public static final KeywordStruct Directory = new KeywordStruct("DIRECTORY");
	/**
	 * An instance of the Keyword Name.
	 */
	public static final KeywordStruct Name = new KeywordStruct("NAME");
	/**
	 * An instance of the Keyword Version.
	 */
	public static final KeywordStruct Version = new KeywordStruct("VERSION");
	/**
	 * An instance of the Keyword Start.
	 */
	public static final KeywordStruct Start = new KeywordStruct("START");
	/**
	 * An instance of the Keyword Start.
	 */
	public static final KeywordStruct End = new KeywordStruct("END");
	/**
	 * An instance of the Keyword Start.
	 */
	public static final KeywordStruct JunkAllowed = new KeywordStruct("JUNK-ALLOWED");

	/** Keywords added for URI support (these are not a part of Common Lisp) */
	/**
	 * An instance of the Keyword News.
	 */
	public static final KeywordStruct News = new KeywordStruct("news");
	/**
	 * An instance of the Keyword File. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordStruct File = new KeywordStruct("file");
	/**
	 * An instance of the Keyword HTTP. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordStruct HTTP = new KeywordStruct("http");
	/**
	 * An instance of the Keyword MAILTO. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordStruct MailTo = new KeywordStruct("mailto");
	/**
	 * An instance of the Keyword HTTPS. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordStruct HTTPS = new KeywordStruct("https");
	/**
	 * An instance of the Keyword PORT. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordStruct Port = new KeywordStruct("PORT");
	/**
	 * An instance of the Keyword USER-INFO. This is added for URI support and is not part of standard Lisp
	 */
	public static final KeywordStruct UserInfo = new KeywordStruct("USER-INFO");

	/** Keywords added for Open.java **/
	/**
	 * An instance of the Keyword Direction.
	 */
	public static final KeywordStruct Direction = new KeywordStruct("DIRECTION");
	/**
	 * An instance of the Keyword Input.
	 */
	public static final KeywordStruct Input = new KeywordStruct("INPUT");
	/**
	 * An instance of the Keyword Output.
	 */
	public static final KeywordStruct Output = new KeywordStruct("OUTPUT");
	/**
	 * An instance of the Keyword IO.
	 */
	public static final KeywordStruct IO = new KeywordStruct("IO");
	/**
	 * An instance of the Keyword Probe.
	 */
	public static final KeywordStruct Probe = new KeywordStruct("PROBE");
	/**
	 * An instance of the Keyword ElementType.
	 */
	public static final KeywordStruct ElementType = new KeywordStruct("ELEMENT-TYPE");
	/**
	 * An instance of the Keyword Character.
	 */
	public static final KeywordStruct Character = new KeywordStruct("CHARACTER");
	/**
	 * An instance of the Keyword UnsignedByte.
	 */
	public static final KeywordStruct UnsignedByte = new KeywordStruct("UNSIGNED-BYTE");
	/**
	 * An instance of the Keyword SignedByte.
	 */
	public static final KeywordStruct SignedByte = new KeywordStruct("SIGNED-BYTE");
	/**
	 * An instance of the Keyword IfExists.
	 */
	public static final KeywordStruct IfExists = new KeywordStruct("IF-EXISTS");
	/**
	 * An instance of the Keyword Error.
	 */
	public static final KeywordStruct Error = new KeywordStruct("ERROR");
	/**
	 * An instance of the Keyword NewVersion.
	 */
	public static final KeywordStruct NewVersion = new KeywordStruct("NEW-VERSION");
	/**
	 * An instance of the Keyword Rename.
	 */
	public static final KeywordStruct Rename = new KeywordStruct("RENAME");
	/**
	 * An instance of the Keyword RenameAndDelete.
	 */
	public static final KeywordStruct RenameAndDelete = new KeywordStruct("RENAME-AND-DELETE");
	/**
	 * An instance of the Keyword Overwrite.
	 */
	public static final KeywordStruct Overwrite = new KeywordStruct("OVERWRITE");
	/**
	 * An instance of the Keyword Append.
	 */
	public static final KeywordStruct Append = new KeywordStruct("APPEND");
	/**
	 * An instance of the Keyword Supersede.
	 */
	public static final KeywordStruct Supersede = new KeywordStruct("SUPERSEDE");
	/**
	 * An instance of the Keyword IfDoesNotExist.
	 */
	public static final KeywordStruct IfDoesNotExist = new KeywordStruct("IF-DOES-NOT-EXIST");
	/**
	 * An instance of the Keyword Create.
	 */
	public static final KeywordStruct Create = new KeywordStruct("CREATE");
	/**
	 * An instance of the Keyword ExternalFormat.
	 */
	public static final KeywordStruct ExternalFormat = new KeywordStruct("EXTERNAL-FORMAT");

}
