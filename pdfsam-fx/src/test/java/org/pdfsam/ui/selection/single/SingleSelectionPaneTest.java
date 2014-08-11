/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/lug/2014
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
package org.pdfsam.ui.selection.single;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;

import javafx.scene.Parent;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.KeyCode;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.HitConsumer;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.commons.OpenFileRequest;
import org.pdfsam.ui.commons.SetDestinationRequest;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.io.ChangedSelectedPdfVersionEvent;

import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class SingleSelectionPaneTest extends GuiTest {

    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public TemporaryFolder tmpFolder = new TemporaryFolder();

    @Override
    protected Parent getRootNode() {
        SingleSelectionPane victim = new SingleSelectionPane(MODULE);
        victim.setId("victim");
        return victim;
    }

    @Test
    public void loadEvent() throws Exception {
        HitTestListener<PdfLoadRequestEvent> listener = new HitTestListener<>();
        eventStudio().add(PdfLoadRequestEvent.class, listener);
        typePathAndValidate();
        assertTrue(listener.isHit());
    }

    @Test
    public void infoMenuItem() throws Exception {
        HitTestListener<ShowPdfDescriptorRequest> listener = new HitTestListener<>();
        eventStudio().add(ShowPdfDescriptorRequest.class, listener);
        typePathAndValidate();
        rightClick(".validable-container-field");
        click(AwesomeIcon.INFO.toString());
        sleep(500);
        assertTrue(listener.isHit());
    }

    @Test
    public void setDestinationMenuItem() throws Exception {
        HitTestListener<SetDestinationRequest> listener = new HitTestListener<SetDestinationRequest>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertFalse(event.isFallback());
            }
        };
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        typePathAndValidate();
        rightClick(".validable-container-field");
        click(AwesomeIcon.FILE_PDF_ALT.toString());
        assertTrue(listener.isHit());
    }

    @Test
    public void openFileMenuItem() throws Exception {
        SingleSelectionPane victim = find("#victim");
        HitTestListener<OpenFileRequest> listener = new HitTestListener<OpenFileRequest>() {
            @Override
            public void onEvent(OpenFileRequest event) {
                super.onEvent(event);
                assertEquals(victim.getPdfDocumentDescriptor().getFile(), event.getFile());
            }
        };
        eventStudio().add(OpenFileRequest.class, listener);
        typePathAndValidate();
        rightClick(".validable-container-field");
        click(AwesomeIcon.FILE_ALT.toString());
        assertTrue(listener.isHit());
    }

    @Test
    public void openFolderMenuItem() throws Exception {
        SingleSelectionPane victim = find("#victim");
        HitTestListener<OpenFileRequest> listener = new HitTestListener<OpenFileRequest>() {
            @Override
            public void onEvent(OpenFileRequest event) {
                super.onEvent(event);
                assertEquals(victim.getPdfDocumentDescriptor().getFile().getParentFile(), event.getFile());
            }
        };
        eventStudio().add(OpenFileRequest.class, listener);
        typePathAndValidate();
        rightClick(".validable-container-field");
        click(AwesomeIcon.FOLDER_OPEN.toString());
        assertTrue(listener.isHit());
    }

    @Test
    public void additionalOnLoadedConsumer() throws Exception {
        HitConsumer<PdfDocumentDescriptor> consumer = new HitConsumer<>();
        SingleSelectionPane victim = find("#victim");
        victim.addOnLoaded(consumer);
        moveToLoadedState(victim);
        assertTrue(consumer.isHit());
    }

    @Test
    public void onLoadedChangedSelectedPdfVersionEvent() throws Exception {
        HitTestListener<ChangedSelectedPdfVersionEvent> listener = new HitTestListener<>();
        eventStudio().add(ChangedSelectedPdfVersionEvent.class, listener, MODULE);
        SingleSelectionPane victim = find("#victim");
        moveToLoadedState(victim);
        assertTrue(listener.isHit());
    }

    @Test
    public void onLoadedSetDestinationRequest() throws Exception {
        HitTestListener<SetDestinationRequest> listener = new HitTestListener<SetDestinationRequest>() {
            @Override
            public void onEvent(SetDestinationRequest event) {
                super.onEvent(event);
                assertTrue(event.isFallback());
            }
        };
        eventStudio().add(SetDestinationRequest.class, listener, MODULE);
        SingleSelectionPane victim = find("#victim");

        moveToLoadedState(victim);
        assertTrue(listener.isHit());
    }

    @Test
    public void disableMenuOnInvalid() throws Exception {
        typePathAndValidate("/this/doesnt/exists");
        ValidableTextField victim = find(".validable-container-field");
        victim.getContextMenu().getItems().parallelStream().filter(i -> !(i instanceof SeparatorMenuItem))
                .forEach(i -> assertTrue(i.isDisable()));
    }

    private void moveToLoadedState(SingleSelectionPane victim) throws Exception {
        typePathAndValidate();
        FXTestUtils.invokeAndWait(() -> {
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
            victim.getPdfDocumentDescriptor().moveStatusTo(PdfDescriptorLoadingStatus.LOADED);
        }, 2);
    }

    private void typePathAndValidate() throws Exception {
        File input = tmpFolder.newFile("chuck.pdf");
        typePathAndValidate(input.getAbsolutePath());
    }

    private void typePathAndValidate(String path) throws Exception {
        ValidableTextField field = find(".validable-container-field");
        // TODO replace with typing when slash works
        FXTestUtils.invokeAndWait(() -> field.setText(path), 2);
        click(field).type(KeyCode.TAB);
    }
}
