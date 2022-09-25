/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.rotate;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import org.junit.ClassRule;
import org.junit.Test;
import org.pdfsam.core.support.KeyStringValueItem;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.control.ComboBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class RotateOptionsPaneTest extends ApplicationTest {

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();
    private RotateOptionsPane victim;

    @Override
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
        ComboBox<KeyStringValueItem<PredefinedSetOfPages>> rotationType = lookup("#rotationType").queryComboBox();
        ComboBox<KeyStringValueItem<Rotation>> rotation = lookup("#rotation").queryComboBox();
        Map<String, String> data = new HashMap<>();
        data.put("rotation", Rotation.DEGREES_270.toString());
        data.put("rotationType", PredefinedSetOfPages.EVEN_PAGES.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(Rotation.DEGREES_270, rotation.getSelectionModel().getSelectedItem().getKey());
        assertEquals(PredefinedSetOfPages.EVEN_PAGES, rotationType.getSelectionModel().getSelectedItem().getKey());
    }

    @Test
    public void reset() {
        ComboBox<KeyStringValueItem<PredefinedSetOfPages>> rotationType = lookup("#rotationType").queryComboBox();
        ComboBox<KeyStringValueItem<Rotation>> rotation = lookup("#rotation").queryComboBox();
        Map<String, String> data = new HashMap<>();
        data.put("rotation", Rotation.DEGREES_270.toString());
        data.put("rotationType", PredefinedSetOfPages.EVEN_PAGES.toString());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.restoreStateFrom(data));
        assertEquals(Rotation.DEGREES_270, rotation.getSelectionModel().getSelectedItem().getKey());
        assertEquals(PredefinedSetOfPages.EVEN_PAGES, rotationType.getSelectionModel().getSelectedItem().getKey());
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals(Rotation.DEGREES_90, rotation.getSelectionModel().getSelectedItem().getKey());
        assertEquals(PredefinedSetOfPages.ALL_PAGES, rotationType.getSelectionModel().getSelectedItem().getKey());
    }
}
