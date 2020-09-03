# Quick reference: gradle

## Requirements

**Ideal/Minimum**

- `gradle` buildtool installed OR gradle wrappers present in your project (`gradlew`/`gradlew.bat`)
- Gradle buildscript present in your project, e.g., `build.gradle` or `build.gradle.kts`

## Project discovery

Directories that contain a gradle buildscript are treated as gradle projects. Gradle buildscripts in project subdirectories are ignored.
