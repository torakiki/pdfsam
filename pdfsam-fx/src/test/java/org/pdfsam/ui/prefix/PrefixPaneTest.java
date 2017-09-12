/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/dic/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.ui.prefix;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.test.ClearEventStudioRule;

import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@SuppressWarnings({ "rawtypes", "unchecked" })
public class PrefixPaneTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();

    private MultipleOutputTaskParametersBuilder builder;
    private Consumer<String> onError;

    @Before
    public void setUp() {
        builder = mock(MultipleOutputTaskParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    protected Parent getRootNode() {
        PrefixPane victim = new PrefixPane();
        victim.setId("victim");
        return victim;
    }

    @Test
    public void apply() throws Exception {
        PrefixPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.apply(builder, onError), 2);
        verify(onError, never()).accept(anyString());
        verify(builder).prefix("PDFsam_");
    }

    @Test
    public void saveState() {
        PrefixPane victim = find(".pdfsam-container");
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("PDFsam_", data.get("victimprefix"));
    }

    @Test
    public void restoreState() throws Exception {
        PrefixPane victim = find(".pdfsam-container");
        Map<String, String> data = new HashMap<>();
        data.put("victimprefix", "Chuck");
        FXTestUtils.invokeAndWait(() -> victim.restoreStateFrom(data), 2);
        assertEquals("Chuck", victim.getText());
    }

    @Test
    public void reset() throws Exception {
        click(p -> p instanceof PrefixField);
        type("newPref");
        PrefixPane victim = find(".pdfsam-container");
        FXTestUtils.invokeAndWait(() -> victim.resetView(), 2);
        assertEquals("PDFsam_", victim.getText());
    }
}
