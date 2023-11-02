/*
 * This file is part of the PDF Split And Merge source code
 * Created on 11 feb 2021
 * Copyright 2021 by Sober Lemur S.r.l. (info@soberlemur.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.pdfsam.core.context;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static java.util.Optional.empty;
import static java.util.Optional.of;

/**
 * @author Andrea Vacondio
 */
@Execution(ExecutionMode.SAME_THREAD)
public class ApplicationRuntimeStateTest {

    private ApplicationRuntimeState victim;

    @BeforeEach
    public void setUp() {
        victim = new ApplicationRuntimeState();
    }

    @Test
    @DisplayName("Existing working Path")
    public void positiveMaybeWorkingPath(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.maybeWorkingPath(tempDir);
        testListener.assertValuesOnly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("Existing working path as String")
    public void positiveMaybeWorkingPathString(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.maybeWorkingPath(tempDir.toString());
        testListener.assertValuesOnly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("Null working Path")
    public void nullMaybeWorkingPath(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.maybeWorkingPath(tempDir);
        victim.maybeWorkingPath((Path) null);
        testListener.assertValuesOnly(empty(), of(tempDir), empty());
    }

    @Test
    @DisplayName("Null working path as String")
    public void nullMaybeWorkingPathString(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.maybeWorkingPath(tempDir);
        victim.maybeWorkingPath((String) null);
        testListener.assertValuesOnly(empty(), of(tempDir), empty());
    }

    @Test
    @DisplayName("Blank working path as String")
    public void blankMaybeWorkingPathString(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.maybeWorkingPath(tempDir);
        victim.maybeWorkingPath("  ");
        testListener.assertValuesOnly(empty(), of(tempDir), empty());
    }

    @Test
    @DisplayName("File working Path")
    public void fileMaybeWorkingPath(@TempDir Path tempDir) throws IOException {
        var testListener = victim.workingPath().test();
        victim.maybeWorkingPath(tempDir);
        var another = Files.createTempFile(tempDir, "test", ".tmp");
        victim.maybeWorkingPath(another);
        testListener.assertValuesOnly(empty(), of(tempDir), of(another.getParent()));
    }

    @Test
    @DisplayName("Default working Path")
    public void defaultWorkingPath(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.defaultWorkingPath(tempDir);
        testListener.assertValuesOnly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("File working Path with default already set")
    public void maybeWorkingPathWithDefault(@TempDir Path tempDir) throws IOException {
        var testListener = victim.workingPath().test();
        victim.defaultWorkingPath(tempDir);
        var another = Files.createTempFile(tempDir, "test", ".tmp");
        victim.maybeWorkingPath(another);
        testListener.assertValuesOnly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("File working String with default already set")
    public void maybeWorkingPathStringWithDefault(@TempDir Path tempDir) throws IOException {
        var testListener = victim.workingPath().test();
        victim.defaultWorkingPath(tempDir);
        var another = Files.createTempFile(tempDir, "test", ".tmp");
        victim.maybeWorkingPath(another.toString());
        testListener.assertValuesOnly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("Removing default working Path")
    public void removeWorkingPathWithDefault(@TempDir Path tempDir) throws IOException {
        var testListener = victim.workingPath().test();
        victim.defaultWorkingPath(tempDir);
        var another = Files.createTempFile(tempDir, "test", ".tmp");
        victim.maybeWorkingPath(another);
        testListener.assertValuesOnly(empty(), of(tempDir));
        testListener.dispose();
        var anotherListener = victim.workingPath().test();
        victim.defaultWorkingPath(null);
        victim.maybeWorkingPath(another);
        anotherListener.assertValuesOnly(of(tempDir), empty(), of(another.getParent()));
    }

    @Test
    public void close() {
        var testListenerTheme = victim.theme().test();
        var testListenerWorkingPath = victim.workingPath().test();
        victim.close();
        testListenerTheme.assertComplete();
        testListenerWorkingPath.assertComplete();
    }

}
