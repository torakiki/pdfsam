/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
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
package org.pdfsam.ui.selection;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class PasswordFieldPopupTest extends GuiTest {
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule("LogStage");
    private PdfDocumentDescriptor pdfDescriptor;

    @Override
    protected Parent getRootNode() {
        pdfDescriptor = mock(PdfDocumentDescriptor.class);
        Button button = new Button("press");
        PasswordFieldPopup victim = new PasswordFieldPopup("module");
        victim.setId("victim");
        button.setOnAction(e -> victim.showFor(button, pdfDescriptor, 0, 0));
        return button;
    }

    @Test
    public void pwdSentOnEnterKey() {
        click("press");
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        type("myPwd").type(KeyCode.ENTER);
        verify(listener).onEvent(any());
        verify(pdfDescriptor).setPassword("myPwd");
    }

    @Test
    public void pwdSentOnButtonPressed() {
        click("press");
        Listener<PdfLoadRequestEvent> listener = mock(Listener.class);
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        type("myPwd").click(".pdfsam-button");
        verify(listener).onEvent(any());
        verify(pdfDescriptor).setPassword("myPwd");

    }
}
