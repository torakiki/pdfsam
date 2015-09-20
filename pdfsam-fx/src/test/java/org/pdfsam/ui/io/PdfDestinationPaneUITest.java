/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import static org.mockito.Mockito.verify;

import java.util.Set;
import java.util.function.Consumer;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.pdfsam.context.UserContext;
import org.pdfsam.support.params.AbstractPdfOutputParametersBuilder;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.parameter.base.AbstractPdfOutputParameters;

import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.CheckBox;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@RunWith(MockitoJUnitRunner.class)
public class PdfDestinationPaneUITest extends GuiTest {
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

    @Override
    protected Parent getRootNode() {
        BrowsablePdfInputField destination = new BrowsablePdfInputField();
        PdfDestinationPane victim = new PdfDestinationPane(destination, MODULE, userContext);
        victim.getStyleClass().add("victim");
        return victim;
    }

    @Test
    public void applyDefault() {
        PdfDestinationPane victim = find(".victim");
        victim.apply(builder, onError);
        verify(builder).compress(false);
        verify(builder).overwrite(false);
    }

    @Test
    public void applyCompress() {
        PdfDestinationPane victim = find(".victim");
        click(n -> n instanceof PdfVersionConstrainedCheckBox);
        victim.apply(builder, onError);
        verify(builder).compress(true);
    }

    @Test
    public void applyClickAll() {
        PdfDestinationPane victim = find(".victim");
        Set<Node> nodes = findAll(n -> n instanceof CheckBox, victim);
        nodes.forEach(n -> click(n));
        victim.apply(builder, onError);
        verify(builder).compress(true);
        verify(builder).overwrite(true);
    }
}
