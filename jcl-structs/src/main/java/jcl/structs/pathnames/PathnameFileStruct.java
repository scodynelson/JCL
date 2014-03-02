package jcl.structs.pathnames;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

public class PathnameFileStruct extends PathnameStruct {

	private final PathnameDirectory directoryWithDirections;

	private static final Pattern PATHNAME_PATTERN = Pattern.compile(File.separator);

	public PathnameFileStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
							  final PathnameName name, final PathnameType type, final PathnameVersion version) {
		super(host, device, directory, name, type, version);
		directoryWithDirections = getDirectoryWithDirections(directory);
	}

	public PathnameFileStruct(final String pathname) {
		this(getHost(), getDevice(pathname), getDirectory(pathname), getName(pathname), getType(pathname), getVersion());
	}

	@Override
	public PathnameDirectory getPathnameDirectory() {
		return directoryWithDirections;
	}

	@Override
	public String toString() {
		return "PathnameFileStruct{"
				+ "\nhost=" + host
				+ ", \ndevice=" + device
				+ ", \ndirectory=" + directory
				+ ", \ndirectoryWithDirections=" + directoryWithDirections
				+ ", \nname=" + name
				+ ", \ntype=" + type
				+ ", \nversion=" + version
				+ '}';
	}

	private static PathnameHost getHost() {
		return new PathnameHost(null);
	}

	private static PathnameDevice getDevice(final String pathname) {
		final String realPathname = resolveUserHome(pathname);
		final String pathPrefix = FilenameUtils.getPrefix(realPathname);
		return new PathnameDevice(pathPrefix);
	}

	private static PathnameDirectory getDirectory(final String pathname) {
		final String realPathname = resolveUserHome(pathname);
		final String directoryPath = FilenameUtils.getFullPathNoEndSeparator(realPathname);
		final String[] tokens = PATHNAME_PATTERN.split(directoryPath);

		final List<String> directoryStrings = Arrays.asList(tokens);

		final Path path = Paths.get(realPathname);
		if (path.isAbsolute()) {
			return new PathnameDirectory(directoryStrings, PathnameDirectoryType.ABSOLUTE);
		} else {
			return new PathnameDirectory(directoryStrings, PathnameDirectoryType.RELATIVE);
		}
	}

	private static PathnameName getName(final String pathname) {
		final String realPathname = resolveUserHome(pathname);
		final String baseName = FilenameUtils.getBaseName(realPathname);
		return new PathnameName(baseName);

		//FilenameUtils will muck up the name here, it should just be NIL
		// There is a special case where FilenameUtils does not properly
		// handle a string of the form "~username" this method performs
		// a check for that case.
		// TODO: Check this case...
//		if (!realPathname.contains(File.separator)
//				&& (realPathname.length() > 1)
//				&& (realPathname.charAt(0) == '~')) {
//			name = null;
//		}

		// FilenameUtils will muck up the name here, it should just be NIL
		// FilenameUtils does not properly handle a string of the
		// form "C:" (when it is just a drive letter and nothing else)
		// so we check for that case here.
		// TODO: Check this case...
//		if ((realPathname.length() == 2)
//				&& (realPathname.charAt(1) == ':')) {
//			name = null;
//		}
	}

	private static PathnameType getType(final String pathname) {
		final String realPathname = resolveUserHome(pathname);
		final String fileExtension = FilenameUtils.getExtension(realPathname);
		return new PathnameType(fileExtension);
	}

	private static PathnameVersion getVersion() {
		return new PathnameVersion(null);
	}

	private static PathnameDirectory getDirectoryWithDirections(final PathnameDirectory pathnameDirectory) {
		final List<String> directory = pathnameDirectory.getDirectory();
		final PathnameDirectoryType directoryType = pathnameDirectory.getPathnameDirectoryType();

		final List<String> directoryStringsWithDirections = new ArrayList<>(directory.size());
		for (final String token : directory) {
			//Leave ".." in the directory list and convert any :BACK encountered
			//to a ".." in directory list to maintain functionality with other functions
			if ("..".equals(token)) {
				final String currentPathString = StringUtils.join(directoryStringsWithDirections, File.separator);
				final Path currentPath = Paths.get(currentPathString);

				// Back is for absolutes / up is for symbolic links
				if (Files.isSymbolicLink(currentPath)) {
					directoryStringsWithDirections.add(PathnameDirectoryDirectionType.UP.toString());
				} else {
					directoryStringsWithDirections.add(PathnameDirectoryDirectionType.BACK.toString());
				}

			} else {
				directoryStringsWithDirections.add(token);
			}
		}
		return new PathnameDirectory(directoryStringsWithDirections, directoryType);
	}

	private static String resolveUserHome(final String pathname) {
		//there are special situations involving tildes in pathname
		//handle a tilde at start of pathname; expand it to the user's directory

		final String realPathname;
		if ("~".equals(pathname)) {
			realPathname = System.getProperty("user.home");
		} else if (pathname.startsWith("~/") || pathname.startsWith("~\\")) {
			realPathname = pathname.replace("~", System.getProperty("user.home"));
		} else {
			realPathname = pathname;
		}
		return FilenameUtils.separatorsToSystem(realPathname);
	}
}
