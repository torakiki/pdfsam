package org.pdfsam.tools.backpages;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Consumer;

import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 23/11/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
@ExtendWith({ ApplicationExtension.class })
class AddBackpagesPaneTest {

    private static final String TOOL_ID = "AddBackpages";

    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(TOOL_ID);
    private AddBackpagesParametersBuilder builder;
    private Consumer<String> onError;
    private AddBackpagesPane victim;
    private FxRobot robot;
    @TempDir
    private Path folder;

    @BeforeEach
    public void setUp() {
        builder = mock(AddBackpagesParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Start
    public void start(Stage stage) {
        victim = new AddBackpagesPane(TOOL_ID);
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validRange() throws Exception {
        populate();
        robot.clickOn("#selectedBackpages").type(KeyCode.DIGIT5, KeyCode.MINUS, KeyCode.DIGIT9).push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(builder).ranges(anySet());
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void emptyRange() throws Exception {
        populate();
        robot.clickOn("#selectedBackpages").push(KeyCode.ENTER);
        victim.apply(builder, onError);
        //empty set is passed
        verify(builder).ranges(anySet());
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void invalidRange() throws Exception {
        populate();
        robot.clickOn("#selectedBackpages").write("Chuck").push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).ranges(anySet());
    }

    @Test
    public void validPace() throws Exception {
        populate();
        robot.clickOn("#repeatPace").type(KeyCode.BACK_SPACE, KeyCode.DIGIT2).push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(builder).step(eq(2));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void emptyPace() throws Exception {
        populate();
        robot.clickOn("#repeatPace").type(KeyCode.BACK_SPACE).push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(builder).step(eq(1));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void invalidPace() throws Exception {
        populate();
        robot.clickOn("#repeatPace").write("Chuck").push(KeyCode.ENTER);
        victim.apply(builder, onError);
        verify(onError).accept(anyString());
        verify(builder, never()).step(anyInt());
    }

    private void populate() throws Exception {
        File file = Files.createTempFile(folder, null, "temp.pdf").toFile();
        var loadEvent = new PdfLoadRequest(TOOL_ID);
        loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(file));
        eventStudio().broadcast(loadEvent, TOOL_ID);
    }
}