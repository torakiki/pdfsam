/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/dic/2014
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
import org.pdfsam.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.test.ClearEventStudioRule;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
@SuppressWarnings({ "rawtypes", "unchecked" })
public class PrefixPaneTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();

    private MultipleOutputTaskParametersBuilder builder;
    private Consumer<String> onError;
    private PrefixPane victim;

    @Before
    public void setUp() {
        builder = mock(MultipleOutputTaskParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Override
    public void start(Stage stage) {
        victim = new PrefixPane();
        victim.setId("victim");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void apply() {
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).prefix("PDFsam_");
    }

    @Test
    public void saveState() {
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("PDFsam_", data.get("victimprefix"));
    }

    @Test
    public void restoreState() {
        Map<String, String> data = new HashMap<>();
        data.put("victimprefix", "Chuck");
        victim.restoreStateFrom(data);
        assertEquals("Chuck", victim.getText());
    }

    @Test
    public void reset() {
        clickOn(p -> p instanceof PrefixField).write("newPref");
        victim.resetView();
        assertEquals("PDFsam_", victim.getText());
    }
}
