/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.tools.rotate;

import javafx.scene.Scene;
import javafx.scene.control.ComboBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.test.ClearEventStudioExtension;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * @author Andrea Vacondio
 *
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class RotateOptionsPaneTest   {

    private RotateOptionsPane victim;

    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        victim = new RotateOptionsPane();
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void validSteps() {
        RotateParametersBuilder builder = mock(RotateParametersBuilder.class);
        Consumer<String> onError = mock(Consumer.class);
        victim.apply(builder, onError);
        verify(builder).rotation(eq(Rotation.DEGREES_90));
        verify(builder).rotationType(eq(PredefinedSetOfPages.ALL_PAGES));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void onSaveWorkspace() {
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals(Rotation.DEGREES_90.toString(), data.get("rotation"));
        assertEquals(PredefinedSetOfPages.ALL_PAGES.toString(), data.get("rotationType"));
    }

    @Test
    public void restoreStateFrom() {
        ComboBox<ComboItem<PredefinedSetOfPages>> rotationType = robot.lookup("#rotationType").queryComboBox();
        ComboBox<ComboItem<Rotation>> rotation = robot.lookup("#rotation").queryComboBox();
        Map<String, String> data = new HashMap<>();
        data.put("rotation", Rotation.DEGREES_270.toString());
        data.put("rotationType", PredefinedSetOfPages.EVEN_PAGES.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(Rotation.DEGREES_270, rotation.getSelectionModel().getSelectedItem().key());
        assertEquals(PredefinedSetOfPages.EVEN_PAGES, rotationType.getSelectionModel().getSelectedItem().key());
    }

    @Test
    public void reset() {
        ComboBox<ComboItem<PredefinedSetOfPages>> rotationType = robot.lookup("#rotationType").queryComboBox();
        ComboBox<ComboItem<Rotation>> rotation = robot.lookup("#rotation").queryComboBox();
        Map<String, String> data = new HashMap<>();
        data.put("rotation", Rotation.DEGREES_270.toString());
        data.put("rotationType", PredefinedSetOfPages.EVEN_PAGES.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(Rotation.DEGREES_270, rotation.getSelectionModel().getSelectedItem().key());
        assertEquals(PredefinedSetOfPages.EVEN_PAGES, rotationType.getSelectionModel().getSelectedItem().key());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals(Rotation.DEGREES_90, rotation.getSelectionModel().getSelectedItem().key());
        assertEquals(PredefinedSetOfPages.ALL_PAGES, rotationType.getSelectionModel().getSelectedItem().key());
    }
}
