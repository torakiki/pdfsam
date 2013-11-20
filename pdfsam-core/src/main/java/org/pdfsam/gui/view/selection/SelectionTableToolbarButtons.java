/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/giu/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.view.selection;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.io.File;

import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.KeyStroke;

import org.bushe.swing.event.EventBus;
import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.event.ModuleEvent;
import org.pdfsam.gui.event.String;
import org.pdfsam.gui.event.EventSubscriberCallback;
import org.pdfsam.gui.support.SharedJFileChooser;
import org.pdfsam.gui.view.AbstractActionWithNamespace;
import org.pdfsam.gui.view.JButtonWithNamespace;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadCompletedEvent;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.filter.FileFilterType;

import static org.pdfsam.gui.event.EnableDisableComponentCallback.disableComponent;
import static org.pdfsam.gui.event.EnableDisableComponentCallback.enableComponent;
import static org.pdfsam.gui.event.EventSubscriberTemplate.ifEvent;

/**
 * Factory methods for the move buttons
 * 
 * @author Andrea Vacondio
 * 
 */
final class SelectionTableToolbarButtons {

    private SelectionTableToolbarButtons() {
        // utility
    }

    public static JButtonWithNamespace moveUpButton(String namespace) {
        return new MoveSelectionButton(new MoveUpSelectedAction(namespace), namespace);
    }

    public static JButtonWithNamespace moveDownButton(String namespace) {
        return new MoveSelectionButton(new MoveDownSelectedAction(namespace), namespace);
    }

    public static JButtonWithNamespace removeButton(String namespace) {
        return new AnySelectionToolbarButton(new RemoveSelectedAction(namespace), namespace);
    }

    public static JButtonWithNamespace clearButton(String namespace) {
        return new SelectionToolbarButton(new ClearSelectionTableAction(namespace), namespace);
    }

    public static JButtonWithNamespace addButton(String namespace) {
        return new SelectionToolbarButton(new AddAction(namespace), namespace);
    }

    /**
     * Button moving selected items
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class MoveSelectionButton extends JButtonWithNamespace {

        public MoveSelectionButton(MoveSelectedAction a, String namespace) {
            super(a, namespace);
            setEnabled(false);
            AnnotationProcessor.process(this);
        }

        @EventSubscriber
        public void disableIfCannotMoveDown(final SelectionChangedEvent event) {
            ifEvent(event).routesTo(getOwnerModule()).execute(new EventSubscriberCallback() {
                public void exec(ModuleEvent e) {
                    setEnabled(event.canMove(((MoveSelectedAction) getAction()).getType()));
                }
            });

        }
    }

    /**
     * Button enabled by any kind of selection (multiple or single) in the selection table
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class AnySelectionToolbarButton extends JButtonWithNamespace {

        public AnySelectionToolbarButton(Action a, String namespace) {
            super(a, namespace);
            setEnabled(false);
            AnnotationProcessor.process(this);
        }

        @EventSubscriber
        public void disableIfNoSelection(final SelectionChangedEvent event) {
            ifEvent(event).routesTo(getOwnerModule()).execute(new EventSubscriberCallback() {
                public void exec(ModuleEvent e) {
                    setEnabled(!event.isClearSelection());
                }
            });
        }
    }

    /**
     * Button displayed in the selection table toolbar which disables itself when the selection table is loading documents
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class SelectionToolbarButton extends JButtonWithNamespace {

        public SelectionToolbarButton(Action a, String namespace) {
            super(a, namespace);
            AnnotationProcessor.process(this);
        }

        @EventSubscriber
        public void disableWhileLoadingDocuments(PdfLoadRequestEvent event) {
            // I'm still loading documents
            ifEvent(event).routesTo(getOwnerModule()).execute(disableComponent(this));
        }

        @EventSubscriber
        public void enableOnLoadDocumentsCompletion(PdfLoadCompletedEvent event) {
            // I'm done loading documents
            ifEvent(event).routesTo(getOwnerModule()).execute(enableComponent(this));
        }

    }

    /**
     * Action to move up selected items in the selection table.
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class MoveUpSelectedAction extends MoveSelectedAction {

        public MoveUpSelectedAction(String namespace) {
            super(namespace, MoveType.UP);
            this.putValue(Action.NAME, DefaultI18nContext.getInstance().i18n("Move Up"));
            this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_UP, InputEvent.ALT_DOWN_MASK));
            this.putValue(Action.SHORT_DESCRIPTION, DefaultI18nContext.getInstance()
                    .i18n("Moves up selected documents"));
            this.putValue(Action.SMALL_ICON, new ImageIcon(this.getClass().getResource("/images/up.png")));
        }
    }

    /**
     * Action to move down selected items in the selection table.
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class MoveDownSelectedAction extends MoveSelectedAction {

        public MoveDownSelectedAction(String namespace) {
            super(namespace, MoveType.DOWN);
            this.putValue(Action.NAME, DefaultI18nContext.getInstance().i18n("Move Down"));
            this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, InputEvent.ALT_DOWN_MASK));
            this.putValue(Action.SHORT_DESCRIPTION,
                    DefaultI18nContext.getInstance().i18n("Moves down selected documents"));
            this.putValue(Action.SMALL_ICON, new ImageIcon(this.getClass().getResource("/images/down.png")));
        }
    }

    /**
     * Action to remove selected items in the selection table.
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class RemoveSelectedAction extends AbstractActionWithNamespace {

        public RemoveSelectedAction(String namespace) {
            super(namespace);
            this.putValue(Action.NAME, DefaultI18nContext.getInstance().i18n("Remove"));
            this.setEnabled(true);
            this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));
            this.putValue(Action.SHORT_DESCRIPTION, DefaultI18nContext.getInstance().i18n("Removes selected documents"));
            this.putValue(Action.SMALL_ICON, new ImageIcon(this.getClass().getResource("/images/remove.png")));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            EventBus.publish(new RemoveSelectedEvent(getOwnerModule()));
        }
    }

    /**
     * Action to clear the selection table.
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class ClearSelectionTableAction extends AbstractActionWithNamespace {

        public ClearSelectionTableAction(String namespace) {
            super(namespace);
            this.putValue(Action.NAME, DefaultI18nContext.getInstance().i18n("Clear"));
            this.setEnabled(true);
            this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, InputEvent.ALT_DOWN_MASK));
            this.putValue(Action.SHORT_DESCRIPTION, DefaultI18nContext.getInstance().i18n("Removes every document"));
            this.putValue(Action.SMALL_ICON, new ImageIcon(this.getClass().getResource("/images/clear.png")));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            EventBus.publish(new ClearSelectionTableEvent(getOwnerModule()));
        }
    }

    /**
     * Action to perform pdf documents loading
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class AddAction extends AbstractActionWithNamespace {

        public AddAction(String namespace) {
            super(namespace);
            this.putValue(Action.NAME, DefaultI18nContext.getInstance().i18n("Add"));
            this.setEnabled(true);
            this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_A, InputEvent.ALT_DOWN_MASK));
            this.putValue(Action.SHORT_DESCRIPTION, DefaultI18nContext.getInstance().i18n("Add documents to the table"));
            this.putValue(Action.SMALL_ICON, new ImageIcon(this.getClass().getResource("/images/add.png")));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            JFileChooser chooser = SharedJFileChooser.getInstance(FileFilterType.PDF,
                    JFileChooser.FILES_AND_DIRECTORIES);
            chooser.setMultiSelectionEnabled(true);
            int retVal = chooser.showOpenDialog(null);

            if (retVal == JFileChooser.APPROVE_OPTION) {
                PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(getOwnerModule());
                for (File current : chooser.getSelectedFiles()) {
                    loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(current));
                }
                EventBus.publish(loadEvent);
            }

        }

    }
}
