/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
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
package org.pdfsam.ui.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Set;
import java.util.function.Consumer;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.pdfsam.core.context.UserContext;
import org.pdfsam.core.support.params.AbstractPdfOutputParametersBuilder;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.io.PdfDestinationPane.DestinationPanelFields;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.parameter.base.AbstractPdfOutputParameters;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.CheckBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(MockitoJUnitRunner.class)
public class PdfDestinationPaneUITest extends ApplicationTest {
    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Mock
    private AbstractPdfOutputParametersBuilder<? extends AbstractPdfOutputParameters> builder;
    @Mock
    private Consumer<String> onError;
    @Mock
    private UserContext userContext;
    private PdfDestinationPane victim;

    @Override
    public void start(Stage stage) {
        when(userContext.isCompressionEnabled()).thenReturn(Boolean.TRUE);
        BrowsablePdfInputField destination = new BrowsablePdfInputField();
        victim = new PdfDestinationPane(destination, MODULE, userContext, true,
                DestinationPanelFields.DISCARD_BOOKMARKS);
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void applyDefault() {
        victim.apply(builder, onError);
        verify(builder).compress(true);
        verify(builder).discardBookmarks(false);
        verify(builder, never()).existingOutput(any());
    }

    @Test
    public void applyCompress() {
        clickOn(n -> n instanceof PdfVersionConstrainedCheckBox);
        victim.apply(builder, onError);
        verify(builder).compress(false);
    }

    @Test
    public void applyClickAll() {
        Set<Node> nodes = lookup(n -> n instanceof CheckBox).queryAll();
        nodes.forEach(n -> clickOn(n));
        victim.apply(builder, onError);
        verify(builder).compress(false);
        verify(builder).existingOutput(ExistingOutputPolicy.OVERWRITE);
        verify(builder).discardBookmarks(true);
    }

    @Test
    public void reset() {
        clickOn(".validable-container-field").write("Chuck");
        Set<Node> nodes = lookup(n -> n instanceof CheckBox).queryAll();
        nodes.forEach(n -> clickOn(n));
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals("", lookup(".validable-container-field").queryAs(ValidableTextField.class).getText());
        assertFalse(victim.overwrite().isSelected());
        assertFalse(lookup("#discardBookmarksField").queryAs(CheckBox.class).isSelected());
        assertTrue(lookup("#compressField").queryAs(CheckBox.class).isSelected());

    }
}
