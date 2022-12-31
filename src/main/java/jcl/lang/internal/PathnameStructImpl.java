/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.StringTokenizer;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BooleanStruct;
import jcl.lang.ConsStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.FileErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link PathnameStructImpl} is the object representation of a Lisp 'pathname' type.
 */
@Log4j2
public class PathnameStructImpl extends LispStructImpl implements PathnameStruct {

	/**
	 * The Unix directory separator character.
	 */
	private static final String DIR_SEPARATOR_UNIX = Character.toString(IOUtils.DIR_SEPARATOR_UNIX);
	/**
	 * The Windows directory separator character.
	 */
	private static final String DIR_SEPARATOR_WINDOWS = Character.toString(IOUtils.DIR_SEPARATOR_WINDOWS);

	/**
	 * The extension separator character.
	 */
	public static final String EXTENSION_SEPARATOR = Character.toString(FilenameUtils.EXTENSION_SEPARATOR);

	private static final String CURRENT_DIR_STRING = ".";

	private static final String CURRENT_DIR_STRING_SLASH = "./";

	private static final String CURRENT_DIR_STRING_BACKSLASH = ".\\";

	private static final String FILE_PREFIX = "file:";

	private static final String JAR_PREFIX = "jar:";
	private static final String JAR_SEPARATOR = "!/";

	/**
	 * Back/Up string for pathname parsing.
	 */
	protected static final String BACK_UP_STRING = "..";

	/**
	 * Back/Up string for pathname parsing.
	 */
	private static final String BACK_UP_STRING_SLASH = "../";

	/**
	 * Wildcard string for pathname parsing.
	 */
	protected static final String WILDCARD_STRING = "*";

	/**
	 * Wildcard-Inferiors string for pathname parsing.
	 */
	protected static final String WILDCARD_INFERIORS_STRING = "**";

	/**
	 * Tilde string for Unix home directories.
	 */
	private static final String TILDE = "~";

	private static final String USER_HOME_PREFIX = "~/";

	private static final char DRIVE_SEPARATOR_CHAR = ':';

	private static final String WINDOWS_UNC_PREFIX_SLASH = "//";
	private static final String WINDOWS_UNC_PREFIX_BACKSLASH = "\\\\";

	protected LispStruct host = NILStruct.INSTANCE;
	protected LispStruct device = NILStruct.INSTANCE;
	protected LispStruct directory = NILStruct.INSTANCE;
	protected LispStruct name = NILStruct.INSTANCE;
	protected LispStruct type = NILStruct.INSTANCE;
	protected LispStruct version = NILStruct.INSTANCE;

	protected String namestring;

	protected PathnameStructImpl() {
	}

	/**
	 * Public constructor.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname object elements
	 */
	public PathnameStructImpl(final String pathname) {
		String s = pathname;
		if (s == null) {
			return;
		}

		if (CURRENT_DIR_STRING.equals(s) || CURRENT_DIR_STRING_SLASH.equals(s)
				|| (SystemUtils.IS_OS_WINDOWS && CURRENT_DIR_STRING_BACKSLASH.equals(s))) {
			directory = ListStruct.toLispList(CommonLispSymbols.RELATIVE_KEYWORD);
			return;
		}

		if (BACK_UP_STRING.equals(s) || BACK_UP_STRING_SLASH.equals(s)) {
			directory = ListStruct.toLispList(CommonLispSymbols.RELATIVE_KEYWORD, CommonLispSymbols.UP_KEYWORD);
			return;
		}

		// A Windows UNC Path
		if (SystemUtils.IS_OS_WINDOWS
				&& (s.startsWith(WINDOWS_UNC_PREFIX_BACKSLASH) || s.startsWith(WINDOWS_UNC_PREFIX_SLASH))) {
			initUNCPath(s);
			return;
		}

		// A JAR file
		if (s.startsWith(JAR_PREFIX) && s.endsWith(JAR_SEPARATOR)) {
			initJarFile(s);
			return;
		}

		// An entry in a JAR file
		final int separatorIndex = s.lastIndexOf(JAR_SEPARATOR);
		if ((separatorIndex > 0) && s.startsWith(JAR_PREFIX)) {
			initJarEntry(s, separatorIndex);
			return;
		}

		// A URL
		if (isValidURL(s)) {
			initURL(s);
			return;
		}

		s = FilenameUtils.separatorsToUnix(s);
		s = expandUserHome(s);

		namestring = s;

		if (SystemUtils.IS_OS_WINDOWS) {
			if ((s.length() >= 2) && (s.charAt(1) == DRIVE_SEPARATOR_CHAR)) {
				final String driveLetter = Character.toString(s.charAt(0));
				device = StringStruct.toLispString(driveLetter);
				s = s.substring(2);
			}
		}

		String directoryString = FilenameUtils.getFullPath(s);
		s = FilenameUtils.getName(s);

		if (StringUtils.isNotEmpty(directoryString)) {
			if (BACK_UP_STRING.equals(s)) {
				directoryString += s;
				s = StringUtils.EMPTY;
			}
			directory = parseDirectory(directoryString);
		}

		// No TYPE can be parsed
		if (s.startsWith(EXTENSION_SEPARATOR)
				&& ((s.indexOf(EXTENSION_SEPARATOR, 1) == -1) || s.endsWith(EXTENSION_SEPARATOR))) {
			name = StringStruct.toLispString(s);
			return;
		}

		final int lastExtensionIndex = s.lastIndexOf(FilenameUtils.EXTENSION_SEPARATOR);
		if (lastExtensionIndex > 0) {
			final String pnName = s.substring(0, lastExtensionIndex);
			if (WILDCARD_STRING.equals(pnName)) {
				name = CommonLispSymbols.WILD_KEYWORD;
			} else {
				name = StringStruct.toLispString(pnName);
			}

			final String pnType = s.substring(lastExtensionIndex + 1);
			if (WILDCARD_STRING.equals(pnType)) {
				type = CommonLispSymbols.WILD_KEYWORD;
			} else {
				type = StringStruct.toLispString(pnType);
			}
		} else if (!s.isEmpty()) {
			if (WILDCARD_STRING.equals(s)) {
				name = CommonLispSymbols.WILD_KEYWORD;
			} else {
				name = StringStruct.toLispString(s);
			}
		}
	}

	private static boolean isValidURL(final String s) {
		// On Windows, the scheme "[A-Z]:.*" is ambiguous; reject as urls
		// This special case reduced exceptions while compiling Maxima by 90%+
		if (SystemUtils.IS_OS_WINDOWS && (s.length() >= 2) && (s.charAt(1) == ':')) {
			final char c = s.charAt(0);
			if ((('A' <= s.charAt(0)) && (s.charAt(0) <= 'Z'))
					|| (('a' <= s.charAt(0)) && (s.charAt(0) <= 'z'))) {
				return false;
			}
		}

		// no schema separator; can't be valid
		if (s.indexOf(':') == -1) {
			return false;
		}

		try {
			final URL url = new URL(s);
		} catch (final MalformedURLException e) {
			// Generating an exception is a heavy operation,
			// we want to try hard not to get into this branch, without
			// implementing the URL class ourselves
			return false;
		}
		return true;
	}

	private static String expandUserHome(final String s) {
		if (SystemUtils.IS_OS_UNIX) {
			final String userHome = SystemUtils.USER_HOME;
			if (TILDE.equals(s)) {
				return userHome + IOUtils.DIR_SEPARATOR_UNIX;
			} else if (s.startsWith(USER_HOME_PREFIX)) {
				return userHome + s.substring(1);
			}
		}
		return s;
	}

	private void initUNCPath(final String s) {
		final int shareIndex;
		final int dirIndex;

		// match \\<server>\<share>\[directories-and-files]
		if (s.startsWith(WINDOWS_UNC_PREFIX_BACKSLASH)) {
			shareIndex = s.indexOf(IOUtils.DIR_SEPARATOR_WINDOWS, 2);
			dirIndex = s.indexOf(IOUtils.DIR_SEPARATOR_WINDOWS, shareIndex + 1);
			// match //<server>/<share>/[directories-and-files]
		} else {
			shareIndex = s.indexOf(IOUtils.DIR_SEPARATOR_UNIX, 2);
			dirIndex = s.indexOf(IOUtils.DIR_SEPARATOR_UNIX, shareIndex + 1);
		}
		if ((shareIndex == -1) || (dirIndex == -1)) {
			throw new ErrorException("Unsupported UNC path format: \"" + s + '"');
		}

		host = StringStruct.toLispString(s.substring(2, shareIndex));
		device = StringStruct.toLispString(s.substring(shareIndex + 1, dirIndex));

		final PathnameStructImpl p = new PathnameStructImpl(s.substring(dirIndex));
		directory = p.directory;
		name = p.name;
		type = p.type;
		version = p.version;
		namestring = null;
	}

	private void initJarFile(String s) {
		ListStruct jars = NILStruct.INSTANCE;
		final int i = s.lastIndexOf(JAR_SEPARATOR, s.length() - JAR_SEPARATOR.length() - 1);
		String jar;
		if (i == -1) {
			jar = s;
		} else {
			// There can be no more than two jar references and the
			// inner one must be a file reference within the outer.
			jar = "jar:file:" + s.substring(i + JAR_SEPARATOR.length());
			s = s.substring(JAR_PREFIX.length(), i + JAR_SEPARATOR.length());
			PathnameStructImpl p = new PathnameStructImpl(s);
			// TODO: casting
			jars = ConsStruct.toLispCons(((ListStruct)p.device).car(), jars);
		}
		if (jar.startsWith("jar:file:")) {
			final String file = jar.substring("jar:file:".length(), jar.length() - JAR_SEPARATOR.length());
			final PathnameStructImpl jarPathname;
			if (!file.isEmpty()) {
				URI uri = null;
				try {
					final URL url = new URL(FILE_PREFIX + file);
					uri = url.toURI();
				} catch (final MalformedURLException | URISyntaxException e) {
					throw new SimpleErrorException("Failed to create URI from "
							                      + '\'' + file + '\''
							                      + ": " + e.getMessage());
				}
				final String path = uri.getPath();
				if (path == null) {
					// We allow "jar:file:baz.jar!/" to construct a relative
					// path for jar files, so MERGE-PATHNAMES means something.
					jarPathname = new PathnameStructImpl(uri.getSchemeSpecificPart());
				} else {
					jarPathname = new PathnameStructImpl(new File(path).getPath());
				}
			} else {
				jarPathname = new PathnameStructImpl("");
			}
			jars = ConsStruct.toLispCons(jarPathname, jars);
		} else {
			URL url = null;
			try {
				url = new URL(jar.substring(JAR_PREFIX.length(), jar.length() - 2));
				final PathnameStructImpl p = new PathnameStructImpl(url.toString());
				jars = ConsStruct.toLispCons(p, jars);
			} catch (final MalformedURLException e) {
				throw new ErrorException("Failed to parse URL "
						                    + '\'' + url + '\''
						                    + e.getMessage());
			}
		}
		jars = jars.nReverse();
		device = jars;
		namestring = null;
	}

	private void initJarEntry(final String s, final int separatorIndex) {
		final String jarURL = s.substring(0, separatorIndex + JAR_SEPARATOR.length());

		final URL url;
		try {
			url = new URL(jarURL);
		} catch (final MalformedURLException e) {
			throw new ErrorException("Failed to parse URL " + '\'' + jarURL + '\'' + e.getMessage(), e);
		}

		final PathnameStructImpl d = new PathnameStructImpl(url.toString());

		device = d.device;

		// Use URI escaping rules
		final String pathnameString = FILE_PREFIX + IOUtils.DIR_SEPARATOR_UNIX + s.substring(separatorIndex + JAR_SEPARATOR.length());
		final PathnameStructImpl p = new PathnameStructImpl(pathnameString);

		directory = p.directory;
		name = p.name;
		type = p.type;
		version = p.version;
	}

	private void initURL(final String s) {
		final URL url;
		try {
			url = new URL(s);
		} catch (final MalformedURLException e) {
			throw new ErrorException("Why?", e); // TODO
		}

		final String scheme = url.getProtocol();
		if ("file".equals(scheme)) {
			initFileURL(s, url);
			return;
		}

		if (scheme == null) {
			throw new ErrorException("Scheme was null for Pathname URL: " + url); // TODO
		}

		final URI uri;
		try {
			uri = url.toURI().normalize();
		} catch (final URISyntaxException e) {
			throw new ErrorException("Couldn't form URI from " + '\'' + url + '\'' + " because: " + e);
		}

		String authority = uri.getAuthority();
		if (authority == null) {
			authority = url.getAuthority();
			if (authority == null) {
				log.warn(String.format("{} has a null authority.", url));
			}
		}

		host = NILStruct.INSTANCE;
		host = ConsStruct.toLispCons(CommonLispSymbols.SCHEME_KEYWORD, host);
		host = ConsStruct.toLispCons(StringStruct.toLispString(scheme), host);

		if (authority != null) {
			host = ConsStruct.toLispCons(CommonLispSymbols.AUTHORITY_KEYWORD, host);
			host = ConsStruct.toLispCons(StringStruct.toLispString(authority), host);
		}

		device = NILStruct.INSTANCE;

		// URI encode necessary characters
		String path = uri.getRawPath();
		if (path == null) {
			path = StringUtils.EMPTY;
		}
		final String query = uri.getRawQuery();
		if (query != null) {
			host = ConsStruct.toLispCons(CommonLispSymbols.QUERY_KEYWORD, host);
			host = ConsStruct.toLispCons(StringStruct.toLispString(query), host);
		}
		final String fragment = uri.getRawFragment();
		if (fragment != null) {
			host = ConsStruct.toLispCons(CommonLispSymbols.FRAGMENT_KEYWORD, host);
			host = ConsStruct.toLispCons(StringStruct.toLispString(fragment), host);
		}
		final PathnameStructImpl p = new PathnameStructImpl(path);

		directory = p.directory;
		name = p.name;
		type = p.type;

		host = ((ListStruct) host).nReverse();
		namestring = null;
	}

	private void initFileURL(final String s, final URL url) {
		final URI uri;
		try {
			uri = new URI(s);
		} catch (final URISyntaxException e) {
			throw new SimpleErrorException("Improper URI syntax for " + '\'' + url + '\'' + ": " + e);
		}

		String uriPath = uri.getPath();
		if (null == uriPath) {
			// Under Windows, deal with pathnames containing
			// devices expressed as "file:z:/foo/path"
			uriPath = uri.getSchemeSpecificPart();
			if ((uriPath == null) || uriPath.isEmpty()) {
				throw new ErrorException("The URI has no path: " + uri);
			}
		}

		final File file = new File(uriPath);
		String path = file.getPath();
		if (uri.toString().endsWith(DIR_SEPARATOR_UNIX) && !path.endsWith(DIR_SEPARATOR_UNIX)) {
			path += DIR_SEPARATOR_UNIX;
		}

		final PathnameStructImpl p = new PathnameStructImpl(path);
		host = p.host;
		device = p.device;
		directory = p.directory;
		name = p.name;
		type = p.type;
		version = p.version;
	}

	private static ListStruct parseDirectory(final String directoryString) {
		if (DIR_SEPARATOR_UNIX.equals(directoryString) || (SystemUtils.IS_OS_WINDOWS && DIR_SEPARATOR_WINDOWS.equals(directoryString))) {
			return ListStruct.toLispList(CommonLispSymbols.ABSOLUTE_KEYWORD);
		}

		ListStruct result;
		if (directoryString.startsWith(DIR_SEPARATOR_UNIX) || (SystemUtils.IS_OS_WINDOWS && directoryString.startsWith(DIR_SEPARATOR_WINDOWS))) {
			result = ListStruct.toLispList(CommonLispSymbols.ABSOLUTE_KEYWORD);
		} else {
			result = ListStruct.toLispList(CommonLispSymbols.RELATIVE_KEYWORD);
		}

		final StringTokenizer st = new StringTokenizer(directoryString, "/\\");
		while (st.hasMoreTokens()) {
			final String token = st.nextToken();
			final LispStruct obj;
			if (WILDCARD_STRING.equals(token)) {
				obj = CommonLispSymbols.WILD_KEYWORD;
			} else if (WILDCARD_INFERIORS_STRING.equals(token)) {
				obj = CommonLispSymbols.WILD_INFERIORS_KEYWORD;
			} else if (BACK_UP_STRING.equals(token)) {
				if (result.car() instanceof StringStruct) {
					result = (ListStruct) result.cdr();
					continue;
				}
				obj = CommonLispSymbols.UP_KEYWORD;
			} else {
				obj = StringStruct.toLispString(token);
			}
			result = ConsStruct.toLispCons(obj, result);
		}
		return result.nReverse();
	}

	/**
	 * Public constructor.
	 *
	 * @param host
	 * 		the pathname host
	 * @param device
	 * 		the pathname device
	 * @param directory
	 * 		the pathname directory
	 * @param name
	 * 		the pathname name
	 * @param type
	 * 		the pathname type
	 * @param version
	 * 		the pathname version
	 */
	public PathnameStructImpl(final LispStruct host, final LispStruct device, final LispStruct directory,
	                          final LispStruct name, final LispStruct type, final LispStruct version) {
		this.host = host;
		this.device = device;
		this.directory = directory;
		this.name = name;
		this.type = type;
		this.version = version;
	}

	@Override
	public LispStruct pathnameHost() {
		return host;
	}

	@Override
	public LispStruct pathnameDevice() {
		return device;
	}

	@Override
	public LispStruct pathnameDirectory() {
		return directory;
	}

	@Override
	public LispStruct pathnameName() {
		return name;
	}

	@Override
	public LispStruct pathnameType() {
		return type;
	}

	@Override
	public LispStruct pathnameVersion() {
		return version;
	}

	private static String uriEncode(final String s) {
		// The constructor we use here only allows absolute paths, so
		// we manipulate the input and output correspondingly.
		final String u;
		if (s.startsWith("/")) {
			u = new String(s);
		} else {
			u = '/' + s;
		}
		try {
			final URI uri = new URI("file", "", u, "");
			final String result = uri.getRawPath();
			if (!s.startsWith("/")) {
				return result.substring(1);
			}
			return result;
		} catch (final URISyntaxException e) {
			throw new ErrorException(e.getMessage(), e);
		}
	}

	@Override
	public String namestring() {
		if (namestring != null) {
			return namestring;
		}
		if ((name == NILStruct.INSTANCE) && (type != NILStruct.INSTANCE)) {
			if (namestring != null) {
				throw new ErrorException("not null namestring??"); // TODO
			}
			return null;
		}
		if (directory instanceof StringStruct) {
			throw new ErrorException("bad directory??: " + directory); // TODO
		}
		StringBuilder sb = new StringBuilder();
		// "If a pathname is converted to a namestring, the symbols NIL and
		// :UNSPECIFIC cause the field to be treated as if it were empty. That
		// is, both NIL and :UNSPECIFIC cause the component not to appear in
		// the namestring." 19.2.2.2.3.1
		if (host != NILStruct.INSTANCE) {
			if (!((host instanceof StringStruct) || isURL())) {
//				Debug.assertTrue(host1 instanceof StringStruct || isURL());
				throw new ErrorException("bad host??: " + host); // TODO
			}
			if (isURL()) {
				// TODO: check casting
				final LispStruct scheme = ((ListStruct) host).getf(CommonLispSymbols.SCHEME_KEYWORD, NILStruct.INSTANCE);
				final LispStruct authority = ((ListStruct) host).getf(CommonLispSymbols.AUTHORITY_KEYWORD, NILStruct.INSTANCE);
				if (scheme == NILStruct.INSTANCE) {
					throw new ErrorException("must have scheme"); // TODO
				}
				sb.append(scheme);
				sb.append(':');
				if (authority != NILStruct.INSTANCE) {
					sb.append("//");
					sb.append(authority);
				}
			} else {
				// A UNC path
				sb.append("//")
				  .append(host)
				  .append('/');
			}
		}
		boolean uriEncoded = false;
		if (device == NILStruct.INSTANCE) {
		} else if (device == CommonLispSymbols.UNSPECIFIC_KEYWORD) {
		} else if (isJar()) {
			final LispStruct[] jars = ((ConsStruct) device).toArray();
			final StringBuilder prefix = new StringBuilder();
			for (int i = 0; i < jars.length; i++) {
				prefix.append(JAR_PREFIX);
				final LispStruct component = jars[i];
				if (!(component instanceof PathnameStructImpl)) {
					return null; // If DEVICE is a CONS, it should only contain Pathname
				}
				if (!((PathnameStructImpl) component).isURL() && (i == 0)) {
					sb.append(FILE_PREFIX);
					uriEncoded = true;
				}
				final PathnameStructImpl jar = (PathnameStructImpl) component;
				final String encodedNamestring;
				if (uriEncoded) {
					encodedNamestring = uriEncode(jar.namestring());
				} else {
					encodedNamestring = jar.namestring();
				}
				sb.append(encodedNamestring);
				sb.append("!/");
			}
			sb = prefix.append(sb);
		} else if (device instanceof StringStruct) {
			sb.append(device);
			if (host == NILStruct.INSTANCE) {
				sb.append(':'); // non-UNC paths
			}
		} else {
			throw new ErrorException("Bad device??: " + device); // TODO
		}
		String directoryNamestring = getDirectoryNamestring();
		if (uriEncoded) {
			directoryNamestring = uriEncode(directoryNamestring);
		}
		if (isJar()) {
			if (directoryNamestring.startsWith("/")) {
				sb.append(directoryNamestring.substring(1));
			} else {
				sb.append(directoryNamestring);
			}
		} else {
			sb.append(directoryNamestring);
		}
		if (name instanceof StringStruct) {
			final String n = ((StringStruct) name).toJavaString();
			if (n.indexOf('/') >= 0) {
				if (namestring != null) {
					throw new ErrorException("not null namestring??"); // TODO
				}
				return null;
			}
			if (uriEncoded) {
				sb.append(uriEncode(n));
			} else {
				sb.append(n);
			}
		} else if (name == CommonLispSymbols.WILD_KEYWORD) {
			sb.append('*');
		}
		if ((type != NILStruct.INSTANCE) && (type != CommonLispSymbols.UNSPECIFIC_KEYWORD)) {
			sb.append('.');
			if (type instanceof StringStruct) {
				final String t = ((StringStruct) type).toJavaString();
				// Allow Windows shortcuts to include TYPE
				if (!(t.endsWith(".lnk") && SystemUtils.IS_OS_WINDOWS)) {
					if (t.indexOf('.') >= 0) {
						if (namestring != null) {
							throw new ErrorException("not null namestring??"); // TODO
						}
						return null;
					}
				}
				if (uriEncoded) {
					sb.append(uriEncode(t));
				} else {
					sb.append(t);
				}
			} else if (type == CommonLispSymbols.WILD_KEYWORD) {
				sb.append('*');
			} else {
				throw new ErrorException("Bad type??: " + type); // TODO
			}
		}

		if (isURL()) {
			// TODO: check casting
			LispStruct o = ((ListStruct) host).getf(CommonLispSymbols.QUERY_KEYWORD, NILStruct.INSTANCE);
			if (o != NILStruct.INSTANCE) {
				sb.append('?');
				sb.append(o);
			}
			// TODO: check casting
			o = ((ListStruct) host).getf(CommonLispSymbols.FRAGMENT_KEYWORD, NILStruct.INSTANCE);
			if (o != NILStruct.INSTANCE) {
				sb.append('#');
				sb.append(o);
			}
		}

		namestring = sb.toString();
		// XXX Decide if this is necessary
		// if (isURL()) {
		//     namestring = Utilities.uriEncode(namestring);
		// }
		return namestring;
	}

	@Override
	public StringStruct directoryNamestring() {
		return StringStruct.toLispString(getDirectoryNamestring());
	}

	@Override
	public LispStruct fileNamestring() {
		final StringBuilder sb = new StringBuilder();
		if (name instanceof StringStruct) {
			sb.append(((StringStruct) name).toJavaString());
		} else if (name == CommonLispSymbols.WILD_KEYWORD) {
			sb.append(WILDCARD_STRING);
		} else {
			return NILStruct.INSTANCE;
		}
		if (type instanceof StringStruct) {
			sb.append('.');
			sb.append(((StringStruct) type).toJavaString());
		} else if (type == CommonLispSymbols.WILD_KEYWORD) {
			sb.append(".*");
		}
		return StringStruct.toLispString(sb.toString());
	}

	protected String getDirectoryNamestring() {
		validateDirectory();
		final StringBuilder sb = new StringBuilder();
		// "If a pathname is converted to a namestring, the symbols NIL and
		// :UNSPECIFIC cause the field to be treated as if it were empty. That
		// is, both NIL and :UNSPECIFIC cause the component not to appear in
		// the namestring." 19.2.2.2.3.1
		if (directory != NILStruct.INSTANCE) {
			if (directory instanceof ListStruct) {
				ListStruct temp = (ListStruct) directory;
				LispStruct part = temp.car();
				temp = (ListStruct) temp.cdr();
				final char separatorChar = '/';
				if (part == CommonLispSymbols.ABSOLUTE_KEYWORD) {
					sb.append(separatorChar);
				} else if (part == CommonLispSymbols.RELATIVE_KEYWORD) {
					if (temp == NILStruct.INSTANCE) {
						// #p"./"
						sb.append('.');
						sb.append(separatorChar);
					}
					// else: Nothing to do.
				} else {
//				throw new FileErrorException("Unsupported directory component " + part + '.', this); // TODO: send 'this' into error??
					throw new FileErrorException("Unsupported directory component " + part + '.', null);
				}
				while (temp != NILStruct.INSTANCE) {
					part = temp.car();
					if (part instanceof StringStruct) {
						sb.append(((StringStruct) part).toJavaString());
					} else if (part == CommonLispSymbols.WILD_KEYWORD) {
						sb.append(WILDCARD_STRING);
					} else if (part == CommonLispSymbols.WILD_INFERIORS_KEYWORD) {
						sb.append(WILDCARD_INFERIORS_STRING);
					} else if (part == CommonLispSymbols.UP_KEYWORD) {
						sb.append(BACK_UP_STRING);
					} else {
//					throw new FileErrorException("Unsupported directory component " + part + '.', this); // TODO: send 'this' into error??
						throw new FileErrorException("Unsupported directory component " + part + '.', null);
					}
					sb.append(separatorChar);
					temp = (ListStruct) temp.cdr();
				}
			} else if (directory instanceof StringStruct) {
				sb.append(directory);
			}
		}
		return sb.toString();
	}

	private void validateDirectory() {
		if (directory instanceof ListStruct) {
			ListStruct temp = (ListStruct) directory;
			while (temp != NILStruct.INSTANCE) {
				final LispStruct first = temp.car();
				temp = (ListStruct) temp.cdr();
				if ((first == CommonLispSymbols.ABSOLUTE_KEYWORD) || (first == CommonLispSymbols.WILD_INFERIORS_KEYWORD)) {
					final LispStruct second = temp.car();
					if ((second == CommonLispSymbols.UP_KEYWORD) || (second == CommonLispSymbols.BACK_KEYWORD)) {
						final String sb = first + " may not be followed immediately by " + second + '.';
						throw new FileErrorException(sb, null);
					}
				}
			}
		}
	}

	private boolean isURL() {
		return host instanceof ConsStruct;
	}

	private boolean isJar() {
		return device instanceof ConsStruct;
	}

	@Override
	public BooleanStruct wildPathnameP(final LispStruct fieldKey) {
		if (NILStruct.INSTANCE.eq(fieldKey)) {
			return isWild() ? TStruct.INSTANCE : NILStruct.INSTANCE;
		}
		if (fieldKey == CommonLispSymbols.DIRECTORY_KEYWORD) {
			if (directory instanceof ConsStruct) {
				final ConsStruct d = (ConsStruct) directory;
				if (d.stream().anyMatch(CommonLispSymbols.WILD_KEYWORD::eql)) {
					return TStruct.INSTANCE;
				}
				if (d.stream().anyMatch(CommonLispSymbols.WILD_INFERIORS_KEYWORD::eql)) {
					return TStruct.INSTANCE;
				}
			}
			return NILStruct.INSTANCE;
		}
		final LispStruct value;
		if (fieldKey == CommonLispSymbols.HOST_KEYWORD) {
			value = host;
		} else if (fieldKey == CommonLispSymbols.DEVICE_KEYWORD) {
			value = device;
		} else if (fieldKey == CommonLispSymbols.NAME_KEYWORD) {
			value = name;
		} else if (fieldKey == CommonLispSymbols.TYPE_KEYWORD) {
			value = type;
		} else if (fieldKey == CommonLispSymbols.VERSION_KEYWORD) {
			value = version;
		} else {
			throw new ProgramErrorException("Unrecognized keyword " + fieldKey + '.');
		}
		if ((value == CommonLispSymbols.WILD_KEYWORD) || (value == CommonLispSymbols.WILD_INFERIORS_KEYWORD)) {
			return TStruct.INSTANCE;
		} else {
			return NILStruct.INSTANCE;
		}
	}

	@Override
	public LispStruct truename() {
		if (isWild()) {
			throw new FileErrorException("Fundamentally unable to find a truename for any wild pathname.", null);
		}

		final String mergeNamestring = namestring();
		if (mergeNamestring == null) {
			throw new FileErrorException("The file " + this + " does not exist.", null);
		}

		final File file = new File(mergeNamestring);
		if (file.exists()) {
			if (file.isDirectory()) {
				return getDirectoryPathname(file);
			}
			try {
				return PathnameStruct.toPathname(file.getCanonicalPath());
			} catch (final IOException e) {
				throw new FileErrorException(e.getMessage(), e, null);
			}
		}
		throw new FileErrorException("The file " + this + " does not exist.", null);
	}

	private static PathnameStruct getDirectoryPathname(final File file) {
		try {
			String namestring = file.getCanonicalPath();
			if (!namestring.isEmpty()) {
				// ??? do we really want the platform dependent separatorChar?
				if (namestring.charAt(namestring.length() - 1) != File.separatorChar) {
					namestring += File.separator;
				}
			}
			return PathnameStruct.toPathname(namestring);
		} catch (final IOException e) {
			throw new ErrorException(e.getMessage(), e);
		}
	}

	@Override
	public LispStruct probeFile() {
		try {
			return truename();
		} catch (final FileErrorException e) {
			log.debug(e.getMessage(), e);
			return NILStruct.INSTANCE;
		}
	}

	@Override
	public LispStruct deleteFile() {
		if (isWild()) {
			throw new FileErrorException("Fundamentally unable to delete file for any wild pathname.", null);
		}

		final String mergeNamestring = namestring();
		if (mergeNamestring == null) {
			throw new FileErrorException("The file " + this + " does not exist.", null);
		}

		final File file = new File(mergeNamestring);
		if (file.exists()) {
			final boolean result = file.delete();
			return BooleanStruct.toLispBoolean(result);
		}
		throw new FileErrorException("The file " + this + " does not exist.", null);
	}

	@Override
	public LispStruct fileWriteDate() {
		if (isWild()) {
			throw new FileErrorException("Bad place for a wild pathname.", null);
		}

		final String mergeNamestring = namestring();
		if (mergeNamestring == null) {
			throw new FileErrorException("The file " + this + " does not exist.", null);
		}

		final File file = new File(mergeNamestring);
		if (file.exists()) {
			final long lastModified = file.lastModified();
			if (lastModified == 0L) {
				return NILStruct.INSTANCE;
			}
			return IntegerStruct.toLispInteger((lastModified / 1000L) + 2208988800L);
		}
		throw new FileErrorException("The file " + this + " does not exist.", null);
	}

	private boolean isWild() {
		if ((host == CommonLispSymbols.WILD_KEYWORD) || (host == CommonLispSymbols.WILD_INFERIORS_KEYWORD)) {
			return true;
		}
		if ((device == CommonLispSymbols.WILD_KEYWORD) || (device == CommonLispSymbols.WILD_INFERIORS_KEYWORD)) {
			return true;
		}
		if (directory instanceof ConsStruct) {
			final ConsStruct d = (ConsStruct) directory;
			if (d.stream().anyMatch(CommonLispSymbols.WILD_KEYWORD::eql)) {
				return true;
			}
			if (d.stream().anyMatch(CommonLispSymbols.WILD_INFERIORS_KEYWORD::eql)) {
				return true;
			}
			for (final LispStruct lispStruct : d) {
				if (lispStruct instanceof StringStruct) {
					final String s = lispStruct.toString();
					if (s.contains(WILDCARD_STRING)) {
						return true;
					}
				}
			}
		}
		if ((name == CommonLispSymbols.WILD_KEYWORD) || (name == CommonLispSymbols.WILD_INFERIORS_KEYWORD)) {
			return true;
		}
		if (name instanceof StringStruct) {
			if (name.toString().contains(WILDCARD_STRING)) {
				return true;
			}
		}
		if ((type == CommonLispSymbols.WILD_KEYWORD) || (type == CommonLispSymbols.WILD_INFERIORS_KEYWORD)) {
			return true;
		}
		if (type instanceof StringStruct) {
			if (type.toString().contains(WILDCARD_STRING)) {
				return true;
			}
		}
		if ((version == CommonLispSymbols.WILD_KEYWORD) || (version == CommonLispSymbols.WILD_INFERIORS_KEYWORD)) {
			return true;
		}
		return false;
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for {@link PathnameStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link PathnameStruct} value</li>
	 * <li>Constructing a new {@link PathnameStruct} with the built {@link #namestring} value</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		host.generate(generatorState);
		device.generate(generatorState);
		directory.generate(generatorState);
		name.generate(generatorState);
		type.generate(generatorState);
		version.generate(generatorState);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.PATHNAME_STRUCT_NAME,
		                   GenerationConstants.PATHNAME_STRUCT_TO_PATHNAME_METHOD_NAME,
		                   GenerationConstants.PATHNAME_STRUCT_TO_PATHNAME_METHOD_DESC,
		                   true);
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.PATHNAME;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.PATHNAME;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.PATHNAME) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.PATHNAME) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		final BooleanStruct printEscape = CommonLispSymbols.PRINT_ESCAPE_VAR.getVariableValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printEscape.toJavaPBoolean()) {
			stringBuilder.append("#P");
		}
		stringBuilder.append('"');
		stringBuilder.append(namestring());
		stringBuilder.append('"');

		return stringBuilder.toString();
	}
}
