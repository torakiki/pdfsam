/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;

import javafx.scene.Parent;
import javafx.scene.control.ComboBox;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class RotateOptionsPaneTest extends GuiTest {

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();

    private RotateParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(RotateParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        return new RotateOptionsPane();
    }

    @Test
    public void validSteps() {
        RotateOptionsPane victim = find(".pdfsam-container");
        victim.apply(builder, onError);
        verify(builder).rotation(eq(Rotation.DEGREES_90));
        verify(builder).rotationType(eq(PredefinedSetOfPages.ALL_PAGES));
        verify(onError, never()).accept(anyString());
    }

    @Test
    public void onSaveWorkspace() {
        Map<String, String> data = new HashMap<>();
        RotateOptionsPane victim = find(".pdfsam-container");
        victim.saveStateTo(data);
        assertEquals(Rotation.DEGREES_90.toString(), data.get("rotation"));
        assertEquals(PredefinedSetOfPages.ALL_PAGES.toString(), data.get("rotationType"));
    }

    @Test
    public void restoreStateFrom() throws Exception {
        ComboBox<KeyStringValueItem<PredefinedSetOfPages>> rotationType = find("#rotationType");
        ComboBox<KeyStringValueItem<Rotation>> rotation = find("#rotation");
        Map<String, String> data = new HashMap<>();
        data.put("rotation", Rotation.DEGREES_270.toString());
        data.put("rotationType", PredefinedSetOfPages.EVEN_PAGES.toString());
        RotateOptionsPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertEquals(Rotation.DEGREES_270, rotation.getSelectionModel().getSelectedItem().getKey());
        assertEquals(PredefinedSetOfPages.EVEN_PAGES, rotationType.getSelectionModel().getSelectedItem().getKey());
    }

    @Test
    public void reset() throws Exception {
        ComboBox<KeyStringValueItem<PredefinedSetOfPages>> rotationType = find("#rotationType");
        ComboBox<KeyStringValueItem<Rotation>> rotation = find("#rotation");
        Map<String, String> data = new HashMap<>();
        data.put("rotation", Rotation.DEGREES_270.toString());
        data.put("rotationType", PredefinedSetOfPages.EVEN_PAGES.toString());
        RotateOptionsPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertEquals(Rotation.DEGREES_270, rotation.getSelectionModel().getSelectedItem().getKey());
        assertEquals(PredefinedSetOfPages.EVEN_PAGES, rotationType.getSelectionModel().getSelectedItem().getKey());
        FXTestUtils.invokeAndWait(() -> victim.resetView(), 2);
        assertEquals(Rotation.DEGREES_90, rotation.getSelectionModel().getSelectedItem().getKey());
        assertEquals(PredefinedSetOfPages.ALL_PAGES, rotationType.getSelectionModel().getSelectedItem().getKey());
    }
}
