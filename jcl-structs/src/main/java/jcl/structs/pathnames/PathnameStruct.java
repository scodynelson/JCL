package jcl.structs.pathnames;

import jcl.structs.classes.BuiltInClassStruct;
import jcl.types.pathnames.Pathname;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

public abstract class PathnameStruct extends BuiltInClassStruct {

	protected final PathnameHost host;
	protected final PathnameDevice device;
	protected final PathnameDirectory directory;
	protected final PathnameName name;
	protected final PathnameType type;
	protected final PathnameVersion version;

	protected PathnameStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
							 final PathnameName name, final PathnameType type, final PathnameVersion version) {
		super(Pathname.INSTANCE, null, null);
		this.host = host;
		this.device = device;
		this.directory = directory;
		this.name = name;
		this.type = type;
		this.version = version;
	}

	public PathnameHost getPathnameHost() {
		return host;
	}

	public PathnameDevice getPathnameDevice() {
		return device;
	}

	public PathnameDirectory getPathnameDirectory() {
		return directory;
	}

	public PathnameName getPathnameName() {
		return name;
	}

	public PathnameType getPathnameType() {
		return type;
	}

	public PathnameVersion getPathnameVersion() {
		return version;
	}

	private static boolean isURI(final String path) {
		try {
			final URI uri = new URI(path);
			return uri.isAbsolute();
		} catch (final URISyntaxException ignore) {
			return false;
		}
	}

	@Override
	public String toString() {
		return "PathnameStruct{"
				+ "host=" + host
				+ ", device=" + device
				+ ", directory=" + directory
				+ ", name=" + name
				+ ", type=" + type
				+ ", version=" + version
				+ '}';
	}

	// BUILDERS

	public static PathnameStruct buildPathname(final String pathname) {
		if (isURI(pathname)) {
			return new PathnameURIStruct(pathname);
		} else {
			return new PathnameFileStruct(pathname);
		}
	}

	// PathnameStruct Property Objects

	public static final class PathnameHost {

		private final String host;
		private final PathnameComponentType componentType;

		public PathnameHost(final String host) {
			this.host = host;

			if (host == null) {
				componentType = PathnameComponentType.NIL;
			} else if (host.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else {
				componentType = null;
			}
		}

		public String getHost() {
			return host;
		}

		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameHost{"
					+ "\nhost=" + host
					+ ", \ncomponentType=" + componentType
					+ '}';
		}
	}

	public static final class PathnameDevice {

		private final String device;
		private final PathnameComponentType componentType;

		public PathnameDevice(final String device) {
			this.device = device;

			if (device == null) {
				componentType = PathnameComponentType.NIL;
			} else if (device.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else if ("*".equalsIgnoreCase(device)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
		}

		public String getDevice() {
			return device;
		}

		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameDevice{"
					+ "\ndevice=" + device
					+ ", \ncomponentType=" + componentType
					+ '}';
		}
	}

	public enum PathnameDeviceType {
		FILE
	}

	public static final class PathnameDirectory {

		private final List<String> directory;
		private final PathnameDirectoryType pathnameDirectoryType;

		public PathnameDirectory(final List<String> directory, final PathnameDirectoryType pathnameDirectoryType) {
			this.directory = directory;
			this.pathnameDirectoryType = pathnameDirectoryType;
		}

		public List<String> getDirectory() {
			return directory;
		}

		public PathnameDirectoryType getPathnameDirectoryType() {
			return pathnameDirectoryType;
		}

		@Override
		public String toString() {
			return "PathnameDirectory{"
					+ "\ndirectory=" + directory
					+ ", \npathnameDirectoryType=" + pathnameDirectoryType
					+ '}';
		}
	}

	public enum PathnameDirectoryType {
		ABSOLUTE,
		RELATIVE
	}

	public enum PathnameDirectoryDirectionType {
		BACK,
		UP
	}

	public static final class PathnameName {

		private final String name;
		private final PathnameComponentType componentType;

		public PathnameName(final String name) {
			this.name = name;

			if (name == null) {
				componentType = PathnameComponentType.NIL;
			} else if (name.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else if ("*".equalsIgnoreCase(name)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
		}

		public String getName() {
			return name;
		}

		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameName{"
					+ "\nname=" + name
					+ ", \ncomponentType=" + componentType
					+ '}';
		}
	}

	public static final class PathnameType {

		private final String type;
		private final PathnameComponentType componentType;

		public PathnameType(final String type) {
			this.type = type;

			if (type == null) {
				componentType = PathnameComponentType.NIL;
			} else if (type.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else if ("*".equalsIgnoreCase(type)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
		}

		public String getType() {
			return type;
		}

		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameType{"
					+ "\ntype=" + type
					+ ", \ncomponentType=" + componentType
					+ '}';
		}
	}

	public static final class PathnameVersion {

		private final Integer version;
		private final PathnameComponentType componentType;

		public PathnameVersion(final Integer version) {
			this.version = version;

			if (version == null) {
				componentType = PathnameComponentType.NEWEST;
			} else {
				componentType = PathnameComponentType.UNSPECIFIC;
			}
		}

		public Integer getVersion() {
			return version;
		}

		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameVersion{"
					+ "\nversion=" + version
					+ ", \ncomponentType=" + componentType
					+ '}';
		}
	}

	public enum PathnameComponentType {
		UNSPECIFIC,
		WILD,
		NEWEST,
		OLDEST,
		PREVIOUS,
		INSTALLED,
		NIL // TODO: or should we just pass null???
	}

	public enum PathnameCase {
		COMMON,
		LOCAL
	}
}
