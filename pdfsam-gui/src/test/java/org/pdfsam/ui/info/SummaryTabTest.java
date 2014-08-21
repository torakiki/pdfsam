/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ago/2014
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
package org.pdfsam.ui.info;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;

import javafx.scene.Parent;
import javafx.scene.control.TabPane;

import javax.inject.Inject;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.ShowPdfDescriptorRequest;
import org.sejda.model.pdf.PdfMetadataKey;
import org.sejda.model.pdf.PdfVersion;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Scope;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class SummaryTabTest extends GuiTest {
    @Rule
    public ClearEventStudioRule studio = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    @Inject
    private ApplicationContext applicationContext;

    @Configuration
    @Lazy
    static class Config {
        @Bean
        @Scope(value = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
        public SummaryTab tab() {
            return new SummaryTab();
        }
    }

    @Override
    protected Parent getRootNode() {
        TabPane tabPane = new TabPane();
        tabPane.getTabs().addAll(applicationContext.getBean(SummaryTab.class));
        return tabPane;
    }

    @Test
    @DirtiesContext
    public void showRequest() throws Exception {
        File file = folder.newFile();
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        fillDescriptor(descriptor);
        FXTestUtils.invokeAndWait(() -> eventStudio().broadcast(new ShowPdfDescriptorRequest(descriptor)), 1);
        assertInfoIsDisplayed(descriptor);
    }

    @Test
    @DirtiesContext
    public void onLoad() throws Exception {
        File file = folder.newFile();
        PdfDocumentDescriptor descriptor = PdfDocumentDescriptor.newDescriptorNoPassword(file);
        FXTestUtils.invokeAndWait(() -> eventStudio().broadcast(new ShowPdfDescriptorRequest(descriptor)), 1);
        fillDescriptor(descriptor);
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.REQUESTED);
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.LOADING);
        descriptor.moveStatusTo(PdfDescriptorLoadingStatus.LOADED);
        Thread.sleep(1000);
        assertInfoIsDisplayed(descriptor);
    }

    private void fillDescriptor(PdfDocumentDescriptor descriptor) {
        descriptor.putInformation(PdfMetadataKey.TITLE.getKey(), "test.title");
        descriptor.putInformation(PdfMetadataKey.AUTHOR.getKey(), "test.author");
        descriptor.putInformation(PdfMetadataKey.CREATOR.getKey(), "test.creator");
        descriptor.putInformation(PdfMetadataKey.SUBJECT.getKey(), "test.subject");
        descriptor.putInformation("Producer", "test.producer");
        descriptor.putInformation("FormattedCreationDate", "test.creationDate");
        descriptor.setPages(2);
        descriptor.setVersion(PdfVersion.VERSION_1_5);
    }

    private void assertInfoIsDisplayed(PdfDocumentDescriptor descriptor) {
        exists("test.title");
        exists("test.author");
        exists("test.creator");
        exists("test.subject");
        exists("test.producer");
        exists("test.creationDate");
        exists("2");
        exists(descriptor.getVersionString());
        exists(descriptor.getFile().getAbsolutePath());
    }

}
