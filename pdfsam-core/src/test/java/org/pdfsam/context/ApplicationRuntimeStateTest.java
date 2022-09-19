/*
 * This file is part of the PDF Black project
 * Created on 11 feb 2021
 * Copyright 2021 by Sober Lemur S.a.s di Vacondio Andrea (info@soberlemur.com).
 *
 * You are not permitted to distribute it in any form unless explicit
 * consent is given by Sober Lemur S.a.s di Vacondio Andrea.
 * You are not permitted to modify it.
 *
 * PDF Black is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */
package org.pdfsam.context;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.pdfsam.model.tool.Tool;

import java.nio.file.Path;

import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.mockito.Mockito.mock;

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
    @DisplayName("Active tool notified")
    public void positiveActiveTool() {
        var testListener = victim.activeTool().test();
        var tool = mock(Tool.class);
        victim.activeTool(tool);
        testListener.assertValuesOnly(empty(), of(tool));
    }

    @Test
    @DisplayName("Null active tool")
    public void nullActiveTool() {
        var testListener = victim.activeTool().test();
        var tool = mock(Tool.class);
        victim.activeTool(tool);
        victim.activeTool(null);
        testListener.assertValuesOnly(empty(), of(tool), empty());
    }

    @Test
    public void close() {
        var testListenerActiveTool = victim.activeTool().test();
        var testListenerTheme = victim.theme().test();
        var testListenerWorkingPath = victim.workingPath().test();
        victim.close();
        testListenerActiveTool.assertComplete();
        testListenerTheme.assertComplete();
        testListenerWorkingPath.assertComplete();

    }

}
