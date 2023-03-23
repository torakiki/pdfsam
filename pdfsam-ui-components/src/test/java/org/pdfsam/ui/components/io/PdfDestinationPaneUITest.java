/*
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.ui.components.io;

import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.CheckBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.core.support.params.AbstractPdfOutputParametersBuilder;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.io.PdfDestinationPane.DestinationPanelFields;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.parameter.base.AbstractPdfOutputParameters;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import java.util.Set;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.pdfsam.core.context.ApplicationContext.app;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
public class PdfDestinationPaneUITest {
    private static final String MODULE = "MODULE";
    @RegisterExtension
    public ClearEventStudioExtension clearStudio = new ClearEventStudioExtension(MODULE);

    private AbstractPdfOutputParametersBuilder<? extends AbstractPdfOutputParameters> builder;
    private Consumer<String> onError;
    private PdfDestinationPane victim;
    private FxRobot robot;

    @BeforeEach
    public void setUp() {
        this.onError = mock(Consumer.class);
        this.builder = mock(AbstractPdfOutputParametersBuilder.class);

    }

    @Start
    public void start(Stage stage) {
        app().persistentSettings().set(BooleanPersistentProperty.PDF_COMPRESSION_ENABLED, true);
        app().persistentSettings().set(BooleanPersistentProperty.OVERWRITE_OUTPUT, false);
        BrowsablePdfInputField destination = new BrowsablePdfInputField();
        victim = new PdfDestinationPane(destination, MODULE, true, DestinationPanelFields.DISCARD_BOOKMARKS);
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
        robot.clickOn(n -> n instanceof PdfVersionConstrainedCheckBox);
        victim.apply(builder, onError);
        verify(builder).compress(false);
    }

    @Test
    public void applyClickAll() {
        Set<Node> nodes = robot.lookup(n -> n instanceof CheckBox).queryAll();
        nodes.forEach(n -> robot.clickOn(n));
        victim.apply(builder, onError);
        verify(builder).compress(false);
        verify(builder).existingOutput(ExistingOutputPolicy.OVERWRITE);
        verify(builder).discardBookmarks(true);
    }

    @Test
    @Tag("NoHeadless")
    public void reset() {
        robot.clickOn(".validable-container-field").write("Chuck");
        Set<Node> nodes = robot.lookup(n -> n instanceof CheckBox).queryAll();
        nodes.forEach(n -> robot.clickOn(n));
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> victim.resetView());
        assertEquals("", robot.lookup(".validable-container-field").queryAs(ValidableTextField.class).getText());
        assertFalse(victim.overwrite().isSelected());
        assertFalse(robot.lookup("#discardBookmarksField").queryAs(CheckBox.class).isSelected());
        assertTrue(robot.lookup("#compressField").queryAs(CheckBox.class).isSelected());

    }
}
