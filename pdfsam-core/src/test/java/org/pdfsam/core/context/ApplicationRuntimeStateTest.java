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
    public void positiveWorkingPath(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.workingPath(tempDir);
        testListener.assertValuesOnly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("Existing working path as String")
    public void positiveWorkingPathString(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.workingPath(tempDir.toString());
        testListener.assertValuesOnly(empty(), of(tempDir));
    }

    @Test
    @DisplayName("Null working Path")
    public void nullWorkingPath(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.workingPath(tempDir);
        victim.workingPath((Path) null);
        testListener.assertValuesOnly(empty(), of(tempDir), empty());
    }

    @Test
    @DisplayName("Null working path as String")
    public void nullWorkingPathString(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.workingPath(tempDir);
        victim.workingPath((String) null);
        testListener.assertValuesOnly(empty(), of(tempDir), empty());
    }

    @Test
    @DisplayName("Blank working path as String")
    public void blankWorkingPathString(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.workingPath(tempDir);
        victim.workingPath("  ");
        testListener.assertValuesOnly(empty(), of(tempDir), empty());
    }

    @Test
    @DisplayName("File working Path")
    public void fileWorkingPath(@TempDir Path tempDir) {
        var testListener = victim.workingPath().test();
        victim.workingPath(tempDir);
        victim.workingPath(tempDir.resolve("test.tmp"));
        testListener.assertValuesOnly(empty(), of(tempDir), empty());
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
